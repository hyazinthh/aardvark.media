namespace Aardvark.UI

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI

[<AutoOpen>]
module Mutable =

    
    
    type MNumericInput(__initial : Aardvark.UI.NumericInput) =
        inherit obj()
        let mutable __current = __initial
        let _value = ResetMod.Create(__initial.value)
        let _min = ResetMod.Create(__initial.min)
        let _max = ResetMod.Create(__initial.max)
        let _step = ResetMod.Create(__initial.step)
        let _format = ResetMod.Create(__initial.format)
        
        member x.value = _value :> IMod<_>
        member x.min = _min :> IMod<_>
        member x.max = _max :> IMod<_>
        member x.step = _step :> IMod<_>
        member x.format = _format :> IMod<_>
        
        member x.Update(v : Aardvark.UI.NumericInput) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                ResetMod.Update(_value,v.value)
                ResetMod.Update(_min,v.min)
                ResetMod.Update(_max,v.max)
                ResetMod.Update(_step,v.step)
                ResetMod.Update(_format,v.format)
                
        
        static member Create(__initial : Aardvark.UI.NumericInput) : MNumericInput = MNumericInput(__initial)
        static member Update(m : MNumericInput, v : Aardvark.UI.NumericInput) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
        interface IUpdatable<Aardvark.UI.NumericInput> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module NumericInput =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let value =
                { new Lens<Aardvark.UI.NumericInput, Microsoft.FSharp.Core.float>() with
                    override x.Get(r) = r.value
                    override x.Set(r,v) = { r with value = v }
                    override x.Update(r,f) = { r with value = f r.value }
                }
            let min =
                { new Lens<Aardvark.UI.NumericInput, Microsoft.FSharp.Core.float>() with
                    override x.Get(r) = r.min
                    override x.Set(r,v) = { r with min = v }
                    override x.Update(r,f) = { r with min = f r.min }
                }
            let max =
                { new Lens<Aardvark.UI.NumericInput, Microsoft.FSharp.Core.float>() with
                    override x.Get(r) = r.max
                    override x.Set(r,v) = { r with max = v }
                    override x.Update(r,f) = { r with max = f r.max }
                }
            let step =
                { new Lens<Aardvark.UI.NumericInput, Microsoft.FSharp.Core.float>() with
                    override x.Get(r) = r.step
                    override x.Set(r,v) = { r with step = v }
                    override x.Update(r,f) = { r with step = f r.step }
                }
            let format =
                { new Lens<Aardvark.UI.NumericInput, Microsoft.FSharp.Core.string>() with
                    override x.Get(r) = r.format
                    override x.Set(r,v) = { r with format = v }
                    override x.Update(r,f) = { r with format = f r.format }
                }
    [<AbstractClass; System.Runtime.CompilerServices.Extension; StructuredFormatDisplay("{AsString}")>]
    type MLeafValue() =
        abstract member TryUpdate : Aardvark.UI.LeafValue -> bool
        abstract member AsString : string
        
        static member private CreateValue(__model : Aardvark.UI.LeafValue) = 
            match __model with
                | Number(item) -> MNumber(__model, item) :> MLeafValue
                | Text(item) -> MText(__model, item) :> MLeafValue
        
        static member Create(v : Aardvark.UI.LeafValue) =
            ResetMod.Create(MLeafValue.CreateValue v) :> IMod<_>
        
        [<System.Runtime.CompilerServices.Extension>]
        static member Update(m : IMod<MLeafValue>, v : Aardvark.UI.LeafValue) =
            let m = unbox<ResetMod<MLeafValue>> m
            if not (m.GetValue().TryUpdate v) then
                m.Update(MLeafValue.CreateValue v)
    
    and private MNumber(__initial : Aardvark.UI.LeafValue, item : Microsoft.FSharp.Core.int) =
        inherit MLeafValue()
        
        let mutable __current = __initial
        let _item = ResetMod.Create(item)
        member x.item = _item :> IMod<_>
        
        override x.ToString() = __current.ToString()
        override x.AsString = sprintf "%A" __current
        
        override x.TryUpdate(__model : Aardvark.UI.LeafValue) = 
            if System.Object.ReferenceEquals(__current, __model) then
                true
            else
                match __model with
                    | Number(item) -> 
                        __current <- __model
                        _item.Update(item)
                        true
                    | _ -> false
    
    and private MText(__initial : Aardvark.UI.LeafValue, item : Microsoft.FSharp.Core.string) =
        inherit MLeafValue()
        
        let mutable __current = __initial
        let _item = ResetMod.Create(item)
        member x.item = _item :> IMod<_>
        
        override x.ToString() = __current.ToString()
        override x.AsString = sprintf "%A" __current
        
        override x.TryUpdate(__model : Aardvark.UI.LeafValue) = 
            if System.Object.ReferenceEquals(__current, __model) then
                true
            else
                match __model with
                    | Text(item) -> 
                        __current <- __model
                        _item.Update(item)
                        true
                    | _ -> false
    
    
    [<AutoOpen>]
    module MLeafValuePatterns =
        let (|MNumber|MText|) (m : MLeafValue) =
            match m with
            | :? MNumber as v -> MNumber(v.item)
            | :? MText as v -> MText(v.item)
            | _ -> failwith "impossible"
    
    
    
    
    
    
    type MProperties(__initial : Aardvark.UI.Properties) =
        inherit obj()
        let mutable __current = __initial
        let _isExpanded = ResetMod.Create(__initial.isExpanded)
        let _isSelected = ResetMod.Create(__initial.isSelected)
        let _isActive = ResetMod.Create(__initial.isActive)
        
        member x.isExpanded = _isExpanded :> IMod<_>
        member x.isSelected = _isSelected :> IMod<_>
        member x.isActive = _isActive :> IMod<_>
        
        member x.Update(v : Aardvark.UI.Properties) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                ResetMod.Update(_isExpanded,v.isExpanded)
                ResetMod.Update(_isSelected,v.isSelected)
                ResetMod.Update(_isActive,v.isActive)
                
        
        static member Create(__initial : Aardvark.UI.Properties) : MProperties = MProperties(__initial)
        static member Update(m : MProperties, v : Aardvark.UI.Properties) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
        interface IUpdatable<Aardvark.UI.Properties> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Properties =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let isExpanded =
                { new Lens<Aardvark.UI.Properties, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.isExpanded
                    override x.Set(r,v) = { r with isExpanded = v }
                    override x.Update(r,f) = { r with isExpanded = f r.isExpanded }
                }
            let isSelected =
                { new Lens<Aardvark.UI.Properties, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.isSelected
                    override x.Set(r,v) = { r with isSelected = v }
                    override x.Update(r,f) = { r with isSelected = f r.isSelected }
                }
            let isActive =
                { new Lens<Aardvark.UI.Properties, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.isActive
                    override x.Set(r,v) = { r with isActive = v }
                    override x.Update(r,f) = { r with isActive = f r.isActive }
                }
    [<AbstractClass; System.Runtime.CompilerServices.Extension; StructuredFormatDisplay("{AsString}")>]
    type MTree() =
        abstract member TryUpdate : Aardvark.UI.Tree -> bool
        abstract member AsString : string
        
        static member private CreateValue(__model : Aardvark.UI.Tree) = 
            match __model with
                | Node(value, properties, children) -> MNode(__model, value, properties, children) :> MTree
                | Leaf(value) -> MLeaf(__model, value) :> MTree
        
        static member Create(v : Aardvark.UI.Tree) =
            ResetMod.Create(MTree.CreateValue v) :> IMod<_>
        
        [<System.Runtime.CompilerServices.Extension>]
        static member Update(m : IMod<MTree>, v : Aardvark.UI.Tree) =
            let m = unbox<ResetMod<MTree>> m
            if not (m.GetValue().TryUpdate v) then
                m.Update(MTree.CreateValue v)
    
    and private MNode(__initial : Aardvark.UI.Tree, value : Aardvark.UI.LeafValue, properties : Aardvark.UI.Properties, children : Aardvark.Base.plist<Aardvark.UI.Tree>) =
        inherit MTree()
        
        let mutable __current = __initial
        let _value = MLeafValue.Create(value)
        let _properties = MProperties.Create(properties)
        let _children = ResetMapList(children, (fun _ e -> MTree.Create(e)), (fun (m,e) -> MTree.Update(m, e)))
        member x.value = _value
        member x.properties = _properties
        member x.children = _children :> alist<_>
        
        override x.ToString() = __current.ToString()
        override x.AsString = sprintf "%A" __current
        
        override x.TryUpdate(__model : Aardvark.UI.Tree) = 
            if System.Object.ReferenceEquals(__current, __model) then
                true
            else
                match __model with
                    | Node(value,properties,children) -> 
                        __current <- __model
                        _value.Update(value)
                        _properties.Update(properties)
                        _children.Update(children)
                        true
                    | _ -> false
    
    and private MLeaf(__initial : Aardvark.UI.Tree, value : Aardvark.UI.LeafValue) =
        inherit MTree()
        
        let mutable __current = __initial
        let _value = MLeafValue.Create(value)
        member x.value = _value
        
        override x.ToString() = __current.ToString()
        override x.AsString = sprintf "%A" __current
        
        override x.TryUpdate(__model : Aardvark.UI.Tree) = 
            if System.Object.ReferenceEquals(__current, __model) then
                true
            else
                match __model with
                    | Leaf(value) -> 
                        __current <- __model
                        _value.Update(value)
                        true
                    | _ -> false
    
    
    [<AutoOpen>]
    module MTreePatterns =
        let (|MNode|MLeaf|) (m : MTree) =
            match m with
            | :? MNode as v -> MNode(v.value,v.properties,v.children)
            | :? MLeaf as v -> MLeaf(v.value)
            | _ -> failwith "impossible"
    
    
    
    
    
    
    type MTreeModel(__initial : Aardvark.UI.TreeModel) =
        inherit obj()
        let mutable __current = __initial
        let _data = MTree.Create(__initial.data)
        
        member x.data = _data
        
        member x.Update(v : Aardvark.UI.TreeModel) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                MTree.Update(_data, v.data)
                
        
        static member Create(__initial : Aardvark.UI.TreeModel) : MTreeModel = MTreeModel(__initial)
        static member Update(m : MTreeModel, v : Aardvark.UI.TreeModel) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
        interface IUpdatable<Aardvark.UI.TreeModel> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module TreeModel =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let data =
                { new Lens<Aardvark.UI.TreeModel, Aardvark.UI.Tree>() with
                    override x.Get(r) = r.data
                    override x.Set(r,v) = { r with data = v }
                    override x.Update(r,f) = { r with data = f r.data }
                }
