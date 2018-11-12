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
        wc.DownloadData (sprintf "%s/rendering/screenshot/%s?w=256&h=196&samples=8" baseAddress controlId.Value)
            |> Thumbnail.create

// Adds a thumbnail request
let request (id : SlideId) (model : Model) = 
    model |> Lens.update (Model.Lens.story |. Story.Lens.thumbnailRequests) (HSet.add id)

// Removes a thumbnail request
let removeRequest (id : SlideId) (model : Model) =
    model |> Lens.update (Model.Lens.story |. Story.Lens.thumbnailRequests) (HSet.remove id)

let threads (model : Model) =
    // Threads for creating thumbnails
    let thumbnailRequests =

        let thumbnailProc (id : SlideId) =
            proclist {
                do! Async.SwitchToNewThread ()
                yield ThumbnailUpdated (id, create ())
            }

        model.story.thumbnailRequests
            |> HSet.fold (fun p id ->
                p |> ThreadPool.add (string id) (thumbnailProc id)
            ) ThreadPool.empty

    // Thread for keeping the thumbnail of the selected
    // slide updated
    let thumbnailUpdater =

        let updateProc (selected : Slide) =
            let rec proc () =
                proclist {
                    yield ThumbnailUpdated (selected.id, create ())

                    do! Async.Sleep 1000
                    yield! proc ()
                }
            
            proclist {
                do! Async.SwitchToNewThread ()
                yield! proc ()
            }
    
        let spawn (selected : Slide) =
            selected |> updateProc |> ThreadPool.single (sprintf "u%A" selected.id)

        // Only run if there is actually a frame selected
        model.story |> Story.trySelected
                    |> Option.filter Slide.isFrame
                    |> Option.filter (fun _ -> model |> Model.isAnimating |> not)
                    |> Option.map spawn
                    |> Option.defaultValue ThreadPool.empty

    [thumbnailRequests; thumbnailUpdater]
        |> ThreadPool.unionMany
