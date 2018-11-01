namespace Story

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Story

[<AutoOpen>]
module Mutable =

    
    
    type MPresentationParams(__initial : Story.PresentationParams) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Story.PresentationParams> = Aardvark.Base.Incremental.EqModRef<Story.PresentationParams>(__initial) :> Aardvark.Base.Incremental.IModRef<Story.PresentationParams>
        let _view = Provenance.Reduced.Mutable.MCameraView.Create(__initial.view)
        let _rendering = RenderingParametersModel.Mutable.MRenderingParameters.Create(__initial.rendering)
        
        member x.view = _view
        member x.rendering = _rendering
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Story.PresentationParams) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                Provenance.Reduced.Mutable.MCameraView.Update(_view, v.view)
                RenderingParametersModel.Mutable.MRenderingParameters.Update(_rendering, v.rendering)
                
        
        static member Create(__initial : Story.PresentationParams) : MPresentationParams = MPresentationParams(__initial)
        static member Update(m : MPresentationParams, v : Story.PresentationParams) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Story.PresentationParams> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module PresentationParams =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let view =
                { new Lens<Story.PresentationParams, Provenance.Reduced.CameraView>() with
                    override x.Get(r) = r.view
                    override x.Set(r,v) = { r with view = v }
                    override x.Update(r,f) = { r with view = f r.view }
                }
            let rendering =
                { new Lens<Story.PresentationParams, RenderingParametersModel.RenderingParameters>() with
                    override x.Get(r) = r.rendering
                    override x.Set(r,v) = { r with rendering = v }
                    override x.Update(r,f) = { r with rendering = f r.rendering }
                }
    [<AbstractClass; System.Runtime.CompilerServices.Extension; StructuredFormatDisplay("{AsString}")>]
    type MContent() =
        abstract member TryUpdate : Story.Content -> bool
        abstract member AsString : string
        
        static member private CreateValue(__model : Story.Content) = 
            match __model with
                | TextContent -> MTextContent(__model) :> MContent
                | FrameContent(item1, item2, item3) -> MFrameContent(__model, item1, item2, item3) :> MContent
        
        static member Create(v : Story.Content) =
            ResetMod.Create(MContent.CreateValue v) :> IMod<_>
        
        [<System.Runtime.CompilerServices.Extension>]
        static member Update(m : IMod<MContent>, v : Story.Content) =
            let m = unbox<ResetMod<MContent>> m
            if not (m.GetValue().TryUpdate v) then
                m.Update(MContent.CreateValue v)
    
    and private MTextContent(__initial : Story.Content) =
        inherit MContent()
        
        let mutable __current = __initial
        
        override x.ToString() = __current.ToString()
        override x.AsString = sprintf "%A" __current
        
        override x.TryUpdate(__model : Story.Content) = 
            if System.Object.ReferenceEquals(__current, __model) then
                true
            else
                match __model with
                    | TextContent -> 
                        __current <- __model
                        true
                    | _ -> false
    
    and private MFrameContent(__initial : Story.Content, item1 : Provenance.Node, item2 : Story.PresentationParams, item3 : Annotations.Annotations) =
        inherit MContent()
        
        let mutable __current = __initial
        let _item1 = Provenance.Mutable.MNode.Create(item1)
        let _item2 = MPresentationParams.Create(item2)
        let _item3 = Annotations.Mutable.MAnnotations.Create(item3)
        member x.item1 = _item1
        member x.item2 = _item2
        member x.item3 = _item3
        
        override x.ToString() = __current.ToString()
        override x.AsString = sprintf "%A" __current
        
        override x.TryUpdate(__model : Story.Content) = 
            if System.Object.ReferenceEquals(__current, __model) then
                true
            else
                match __model with
                    | FrameContent(item1,item2,item3) -> 
                        __current <- __model
                        Provenance.Mutable.MNode.Update(_item1, item1)
                        MPresentationParams.Update(_item2, item2)
                        Annotations.Mutable.MAnnotations.Update(_item3, item3)
                        true
                    | _ -> false
    
    
    [<AutoOpen>]
    module MContentPatterns =
        let (|MTextContent|MFrameContent|) (m : MContent) =
            match m with
            | :? MTextContent as v -> MTextContent
            | :? MFrameContent as v -> MFrameContent(v.item1,v.item2,v.item3)
            | _ -> failwith "impossible"
    
    
    
    
    
    
    type MSlide(__initial : Story.Slide) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Story.Slide> = Aardvark.Base.Incremental.EqModRef<Story.Slide>(__initial) :> Aardvark.Base.Incremental.IModRef<Story.Slide>
        let _id = ResetMod.Create(__initial.id)
        let _content = MContent.Create(__initial.content)
        let _thumbnail = ResetMod.Create(__initial.thumbnail)
        
        member x.id = _id :> IMod<_>
        member x.content = _content
        member x.thumbnail = _thumbnail :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Story.Slide) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_id,v.id)
                MContent.Update(_content, v.content)
                ResetMod.Update(_thumbnail,v.thumbnail)
                
        
        static member Create(__initial : Story.Slide) : MSlide = MSlide(__initial)
        static member Update(m : MSlide, v : Story.Slide) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Story.Slide> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Slide =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let id =
                { new Lens<Story.Slide, Story.SlideId>() with
                    override x.Get(r) = r.id
                    override x.Set(r,v) = { r with id = v }
                    override x.Update(r,f) = { r with id = f r.id }
                }
            let content =
                { new Lens<Story.Slide, Story.Content>() with
                    override x.Get(r) = r.content
                    override x.Set(r,v) = { r with content = v }
                    override x.Update(r,f) = { r with content = f r.content }
                }
            let thumbnail =
                { new Lens<Story.Slide, Story.Thumbnail>() with
                    override x.Get(r) = r.thumbnail
                    override x.Set(r,v) = { r with thumbnail = v }
                    override x.Update(r,f) = { r with thumbnail = f r.thumbnail }
                }
    
    
    type MStory(__initial : Story.Story) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Story.Story> = Aardvark.Base.Incremental.EqModRef<Story.Story>(__initial) :> Aardvark.Base.Incremental.IModRef<Story.Story>
        let _slides = MList.Create(__initial.slides, (fun v -> MSlide.Create(v)), (fun (m,v) -> MSlide.Update(m, v)), (fun v -> v))
        let _selected = MOption.Create(__initial.selected, (fun v -> MSlide.Create(v)), (fun (m,v) -> MSlide.Update(m, v)), (fun v -> v))
        let _showAnnotations = ResetMod.Create(__initial.showAnnotations)
        
        member x.slides = _slides :> alist<_>
        member x.selected = _selected :> IMod<_>
        member x.showAnnotations = _showAnnotations :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Story.Story) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MList.Update(_slides, v.slides)
                MOption.Update(_selected, v.selected)
                ResetMod.Update(_showAnnotations,v.showAnnotations)
                
        
        static member Create(__initial : Story.Story) : MStory = MStory(__initial)
        static member Update(m : MStory, v : Story.Story) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Story.Story> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Story =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let slides =
                { new Lens<Story.Story, Aardvark.Base.plist<Story.Slide>>() with
                    override x.Get(r) = r.slides
                    override x.Set(r,v) = { r with slides = v }
                    override x.Update(r,f) = { r with slides = f r.slides }
                }
            let selected =
                { new Lens<Story.Story, Microsoft.FSharp.Core.Option<Story.Slide>>() with
                    override x.Get(r) = r.selected
                    override x.Set(r,v) = { r with selected = v }
                    override x.Update(r,f) = { r with selected = f r.selected }
                }
            let showAnnotations =
                { new Lens<Story.Story, System.Boolean>() with
                    override x.Get(r) = r.showAnnotations
                    override x.Set(r,v) = { r with showAnnotations = v }
                    override x.Update(r,f) = { r with showAnnotations = f r.showAnnotations }
                }
