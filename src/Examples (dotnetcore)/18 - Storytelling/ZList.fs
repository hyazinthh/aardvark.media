namespace Aardvark.Base

open System
open System.Collections.Generic

[<DefaultAugmentationAttribute(false)>]
type ZList<'a> =
    | Empty
    | Zipped of 'a list * 'a * 'a list with

    static member Single (value : 'a) =
        Zipped ([], value, [])

    static member OfList (list : 'a list) =
        match list with
            | [] -> Empty
            | x::xs -> Zipped ([], x, xs)

    member x.IsEmpty =
        match x with
            | Empty -> true
            | _ -> false

    member x.Right =
        match x with
            | Empty | Zipped (_, _, []) ->
                None
            | Zipped (left, v, r::right) ->
                Some (Zipped (v::left, r, right))

    member x.Left =
        match x with
            | Empty | Zipped ([], _, _) ->
                None
            | Zipped (l::left, v, right) ->
                Some (Zipped (left, l, v::right))

    member x.TryHead = 
        match x with
            | Empty -> None
            | Zipped (_, v, _) -> Some v

    member x.Head =
        match x.TryHead with
            | None -> raise (ArgumentException ("List is empty"))
            | Some v -> v

    member x.IsBeginning =
        Option.isNone x.Left

    member x.Beginning =
        match x.Left with
            | None -> x
            | Some l -> l.Beginning

    member x.IsEnd =
        Option.isNone x.Right

    member x.End =
        match x.Right with
            | None -> x
            | Some r -> r.End

    member x.Set (value : 'a) =
        match x with
            | Empty -> x
            | Zipped (left, _, right) ->
                Zipped (left, value, right)

    member x.Update (f : 'a -> 'a) =
        match x with
            | Empty -> x
            | Zipped (left, value, right) ->
                Zipped (left, f value, right)

    member x.InsertBefore (value : 'a) =
        match x with
            | Empty -> 
                ZList.Single value
            | Zipped (left, v, right) ->
                Zipped (left, value, v::right)

    member x.InsertAfter (value : 'a) =
        match x with
            | Empty -> 
                ZList.Single value
            | Zipped (left, v, right) ->
                Zipped (v::left, value, right)

    member x.Append (value : 'a) =
        x.End.InsertAfter value

    member x.Map (f : 'a -> 'b) =
        match x with
            | Empty -> Empty
            | Zipped (left, v, right) ->
                Zipped (List.map f left, f v, List.map f right)

    member x.Filter (predicate : 'a -> bool) =
        ZList.OfList (x.ToList |> List.filter predicate)

    member x.Remove (predicate : 'a -> bool) =
        x.Filter (predicate >> not)

    member x.Length =
        match x with
            | Empty -> 0
            | Zipped (left, _, right) -> left.Length + right.Length + 1

    member x.ToList =
        match x with
            | Empty -> []
            | Zipped (left, v, right) -> (List.rev left) @ (v :: right)

    member x.TryFind (predicate : 'a -> bool) =
        let rec fnd (y : 'a zlist) =
            match y with
                | Zipped (_, v, _) when predicate v -> Some y
                | Empty | Zipped (_, _, []) -> None
                | _ ->  fnd y.Right.Value
            
        fnd x.Beginning

    member x.Find (predicate : 'a -> bool) =
        match x.TryFind predicate with
            | None -> raise (KeyNotFoundException ())
            | Some l -> l

    member x.FindDefault (predicate : 'a -> bool) (def : 'a zlist) =
        match x.TryFind predicate with
            | None -> def
            | Some l -> l

    member x.TryFindIndex (predicate : 'a -> bool) =
        x.ToList |> List.tryFindIndex predicate

    member x.FindIndex (predicate : 'a -> bool) =
        match x.TryFindIndex predicate with
            | None -> raise (KeyNotFoundException ())
            | Some i -> i
        
and 'a zlist = ZList<'a>       

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ZList =

    let empty : ZList<'a> = ZList.Empty

    let single (value : 'a) = ZList.Single value

    let ofList (list : 'a list) = ZList.OfList list

    let isEmpty (list : ZList<'a>) = list.IsEmpty

    let right (list : ZList<'a>) = list.Right

    let left (list : ZList<'a>) = list.Left

    let tryHead (list : ZList<'a>) = list.TryHead

    let head (list : ZList<'a>) = list.Head

    let isBeginning (list : ZList<'a>) = list.IsBeginning

    let isEnd (list : ZList<'a>) = list.IsEnd

    let beginning (list : ZList<'a>) = list.Beginning

    let ending (list : ZList<'a>) = list.End

    let set (value : 'a) (list : ZList<'a>) = list.Set value

    let update (f : 'a -> 'a) (list : ZList<'a>) = list.Update f

    let insertBefore (value : 'a) (list : ZList<'a>) = list.InsertBefore value

    let insertAfter (value : 'a) (list : ZList<'a>) = list.InsertAfter value

    let append (value : 'a) (list : ZList<'a>) = list.Append value

    let map (f : 'a -> 'b) (list : ZList<'a>) = list.Map f

    let filter (predicate : 'a -> bool) (list : ZList<'a>) = list.Filter predicate

    let remove (predicate : 'a -> bool) (list : ZList<'a>) = list.Remove predicate

    let length (list : ZList<'a>) = list.Length

    let toList (list : ZList<'a>) = list.ToList

    let tryFind (predicate : 'a -> bool) (list : ZList<'a>) = list.TryFind predicate

    let find (predicate : 'a -> bool) (list : ZList<'a>) = list.Find predicate

    let findDefault (predicate : 'a -> bool) (def : ZList<'a>) (list : ZList<'a>) = list.FindDefault predicate def

    let tryFindIndex (predicate : 'a -> bool) (list : ZList<'a>) = list.TryFindIndex predicate

    let findIndex (predicate : 'a -> bool) (list : ZList<'a>) = list.FindIndex predicate