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
    
    
    type MStory(__initial : Story.Story) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Story.Story> = Aardvark.Base.Incremental.EqModRef<Story.Story>(__initial) :> Aardvark.Base.Incremental.IModRef<Story.Story>
        let _slides = MList.Create(__initial.slides)
        let _current = ResetMod.Create(__initial.current)
        
        member x.slides = _slides :> alist<_>
        member x.current = _current :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Story.Story) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MList.Update(_slides, v.slides)
                ResetMod.Update(_current,v.current)
                
        
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
            let current =
                { new Lens<Story.Story, Aardvark.Base.Index>() with
                    override x.Get(r) = r.current
                    override x.Set(r,v) = { r with current = v }
                    override x.Update(r,f) = { r with current = f r.current }
                }
