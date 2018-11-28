module ThumbnailApp

open System.Net
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.Service.Server

open Model
open Story

[<AutoOpen>]
module private Helpers = 

    let baseAddress = "http://localhost:4321"

    let size = V2i (256, 196)

    let controlId = lazy (
        use wc = new WebClient ()

        let rec get () =
            let result = wc.DownloadString (sprintf "%s/rendering/stats.json" baseAddress)
            let stats : ClientStatistics list =
                    result |> Pickler.unpickleOfJson

            match stats with
                | [] -> get ()
                | x::_ -> x.name

        get ()
    )

    // Creates a thumbnail by taking a screenshot
    let create () =
        use wc = new WebClient ()
        wc.DownloadData (sprintf "%s/rendering/screenshot/%s?w=%d&h=%d&samples=8" baseAddress controlId.Value size.X size.Y)
            |> Thumbnail.create

// Thumbnail size
let size = size

// Adds a thumbnail request
let request (id : SlideId) (model : Model) = 
    model |> Lens.update (Model.Lens.story |. Story.Lens.thumbnailRequests) (HSet.add id)

// Adds a blocking thumbnail request
let syncRequest (id : SlideId) (model : Model) =
    let f s = { s with thumbnail = create () }
    model |> Lens.update Model.Lens.story (Story.tryUpdateById id f)

// Removes a thumbnail request
let removeRequest (id : SlideId) (model : Model) =
    model |> Lens.update (Model.Lens.story |. Story.Lens.thumbnailRequests) (HSet.remove id)

let threads (model : Model) =

    let thumbnailProc (id : SlideId) =
        proclist {
            do! Async.SwitchToNewThread ()
            yield ThumbnailUpdated (id, create ())
        }

    model.story.thumbnailRequests
        |> HSet.fold (fun p id ->
            p |> ThreadPool.add (string id) (thumbnailProc id)
        ) ThreadPool.empty
