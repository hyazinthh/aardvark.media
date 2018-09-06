namespace Story

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Story

[<AutoOpen>]
module Mutable =

    
    
    type MText(__initial : Story.Text) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Story.Text> = Aardvark.Base.Incremental.EqModRef<Story.Text>(__initial) :> Aardvark.Base.Incremental.IModRef<Story.Text>
        let _position = ResetMod.Create(__initial.position)
        let _font = ResetMod.Create(__initial.font)
        let _text = ResetMod.Create(__initial.text)
        
        member x.position = _position :> IMod<_>
        member x.font = _font :> IMod<_>
        member x.text = _text :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Story.Text) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_position,v.position)
                ResetMod.Update(_font,v.font)
                ResetMod.Update(_text,v.text)
                
        
        static member Create(__initial : Story.Text) : MText = MText(__initial)
        static member Update(m : MText, v : Story.Text) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Story.Text> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Text =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let position =
                { new Lens<Story.Text, Aardvark.Base.V2d>() with
                    override x.Get(r) = r.position
                    override x.Set(r,v) = { r with position = v }
                    override x.Update(r,f) = { r with position = f r.position }
                }
            let font =
                { new Lens<Story.Text, Aardvark.Rendering.Text.Font>() with
                    override x.Get(r) = r.font
                    override x.Set(r,v) = { r with font = v }
                    override x.Update(r,f) = { r with font = f r.font }
                }
            let text =
                { new Lens<Story.Text, System.String>() with
                    override x.Get(r) = r.text
                    override x.Set(r,v) = { r with text = v }
                    override x.Update(r,f) = { r with text = f r.text }
                }
    
    
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
    
    
    type MSlide(__initial : Story.Slide) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Story.Slide> = Aardvark.Base.Incremental.EqModRef<Story.Slide>(__initial) :> Aardvark.Base.Incremental.IModRef<Story.Slide>
        let _id = ResetMod.Create(__initial.id)
        let _content = ResetMod.Create(__initial.content)
        
        member x.id = _id :> IMod<_>
        member x.content = _content :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Story.Slide) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_id,v.id)
                ResetMod.Update(_content,v.content)
                
        
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
    
    
    type MStory(__initial : Story.Story) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Story.Story> = Aardvark.Base.Incremental.EqModRef<Story.Story>(__initial) :> Aardvark.Base.Incremental.IModRef<Story.Story>
        let _slides = ResetMod.Create(__initial.slides)
        let _selected = MOption.Create(__initial.selected, (fun v -> MSlide.Create(v)), (fun (m,v) -> MSlide.Update(m, v)), (fun v -> v))
        
        member x.slides = _slides :> IMod<_>
        member x.selected = _selected :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Story.Story) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_slides,v.slides)
                MOption.Update(_selected, v.selected)
                
        
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
                { new Lens<Story.Story, Aardvark.Base.ZList<Story.Slide>>() with
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
