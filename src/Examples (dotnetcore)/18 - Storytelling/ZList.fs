namespace Aardvark.Base

type private ZListLocation<'a> =
    | Empty
    | L of 'a list * 'a * 'a list

type ZList<'a> private (location : ZListLocation<'a>) =

    static member Empty = 
        ZList<'a> Empty

    static member Single (value : 'a) =
        ZList (L ([], value, []))

    static member OfList (list : 'a list) =
        match list with
            | [] -> ZList Empty
            | x::xs -> ZList (L ([], x, xs))

    member private x.Location = location

    member x.IsEmpty =
        match location with
            | Empty -> true
            | _ -> false

    member x.Right =
        match location with
            | Empty | L (_, _, []) -> 
                None
            | L (left, v, r::right) -> 
                Some (ZList (L (v::left, r, right)))

    member x.Left =
        match location with
            | Empty | L ([], _, _) ->
                None
            | L (l::left, v, right) ->
                Some (ZList (L (left, l, v::right)))

    member x.TryHead = 
        match location with
            | Empty -> None
            | L (_, v, _) -> Some v

    member x.Head =
        match x.TryHead with
            | None -> failwith "Trying to get head of empty list"
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

    member x.InsertBefore (value : 'a) =
        match location with
            | Empty -> 
                ZList<_>.Single value
            | L (left, v, right) ->
                ZList (L (left, value, v::right))

    member x.InsertAfter (value : 'a) =
        match location with
            | Empty -> 
                ZList<_>.Single value
            | L (left, v, right) ->
                ZList (L (v::left, value, right))

    member x.Append (value : 'a) =
        x.End.InsertAfter value

    member x.Map (f : 'a -> 'b) =
        match location with
            | Empty ->
                ZList Empty
            | L (left, v, right) ->
                ZList (L (List.map f left, f v, List.map f right))

    member x.Filter (predicate : 'a -> bool) =
        ZList.OfList (x.ToList |> List.filter predicate)

    member x.Remove (predicate : 'a -> bool) =
        x.Filter (predicate >> not)

    member x.Count =
        match location with
            | Empty -> 0
            | L (left, _, right) -> left.Length + right.Length + 1

    member x.ToList =
        match location with
            | Empty -> []
            | L (left, v, right) -> (List.rev left) @ (v :: right)

     member x.TryFind (predicate : 'a -> bool) =
        let rec fnd (y : 'a zlist) =
            match y.Location with
                | L (_, v, _) when predicate v -> Some y
                | Empty | L (_, _, []) -> None
                | _ ->  fnd y.Right.Value
            
        fnd x.Beginning

     member x.Find (predicate : 'a -> bool) =
        match x.TryFind predicate with
            | None -> failwith "Not found"
            | Some l -> l

     member x.FindDefault (predicate : 'a -> bool) (def : 'a zlist) =
        match x.TryFind predicate with
            | None -> def
            | Some l -> l
        
and 'a zlist = ZList<'a>       

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ZList =

    let empty<'a> = ZList<'a>.Empty 

    let single (value : 'a) = ZList<_>.Single value

    let ofList (list : List<'a>) = ZList<_>.OfList list

    let isEmpty (list : ZList<'a>) = list.IsEmpty

    let right (list : ZList<'a>) = list.Right

    let left (list : ZList<'a>) = list.Left

    let tryHead (list : ZList<'a>) = list.TryHead

    let head (list : ZList<'a>) = list.Head

    let isBeginning (list : ZList<'a>) = list.IsBeginning

    let isEnd (list : ZList<'a>) = list.IsEnd

    let beginning (list : ZList<'a>) = list.Beginning

    let ending (list : ZList<'a>) = list.End

    let insertBefore (value : 'a) (list : ZList<'a>) = list.InsertBefore value

    let insertAfter (value : 'a) (list : ZList<'a>) = list.InsertAfter value

    let append (value : 'a) (list : ZList<'a>) = list.Append value

    let map (f : 'a -> 'b) (list : ZList<'a>) = list.Map f

    let filter (predicate : 'a -> bool) (list : ZList<'a>) = list.Filter predicate

    let remove (predicate : 'a -> bool) (list : ZList<'a>) = list.Remove predicate

    let count (list : ZList<'a>) = list.Count

    let toList (list : ZList<'a>) = list.ToList

    let tryFind (predicate : 'a -> bool) (list : ZList<'a>) = list.TryFind predicate

    let find (predicate : 'a -> bool) (list : ZList<'a>) = list.Find predicate

    let findDefault (predicate : 'a -> bool) (def : ZList<'a>) (list : ZList<'a>) = list.FindDefault predicate def
