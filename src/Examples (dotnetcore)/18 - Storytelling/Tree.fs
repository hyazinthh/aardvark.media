namespace Provenance

// The inner representation of the tree as nodes
type private Node<'a> = {
    value : 'a
    children : Node<'a> list
}

// Path type for the zipper
type private Path<'a> =
    | Top of 'a
    | Node of 'a * Node<'a> list * Path<'a> * Node<'a> list

    member x.getValue =
        match x with
            | Top v
            | Node (v, _, _, _) -> v

// Zipper
type private Location<'a> = L of Node<'a> * Path<'a>

// The tree class which is just a wrapper for the
// private zipper
type tree<'a> private (location : Location<'a>) =

    let (L(t, p)) = location

    static member Single value =
        let n = { value = value; children = [] }
        tree (L (n, Top value))

    member x.Value = t.value

    member x.IsRoot =
        match p with
            | Top _ -> true
            | _ -> false

    member x.Left =
        match p with
            | Top _ | Node (_, [], _, _) -> 
                None
            | Node (_, l::left, up, right) -> 
                Some (tree (L (l, Node (l.value, left, up, t::right))))

    member x.Right =
        match p with
            | Top _ | Node (_, _, _, []) -> 
                None
            | Node (_, left, up, r::right) -> 
                Some (tree (L (r, Node (r.value, t::left, up, right))))

    member x.Parent =
        match p with
            | Top _ -> 
                None
            | Node (_, left, up, right) ->
                Some (tree (L ({ value = up.getValue
                                 children = List.rev left @ t::right }, 
                               up)
                            )
                     )

    member x.Child =
        match t.children with
            | [] -> None
            | x::xs -> Some (tree (L(x, Node(x.value, [], p, xs))))

    member x.Children =
        let rec sib accum (t : tree<'a>) =
            match t.Right with
                | None -> t::accum
                | Some r -> sib (t::accum) r

        match x.Child with
            | None -> []
            | Some t -> sib [] t

    member x.Filter predicate = [
            if predicate t.value then yield x
            yield! x.Children 
                        |> List.collect (fun t -> t.Filter predicate)
        ]

    // Find methods use filter, may be optimized
    member x.TryFind predicate =
        match x.Filter predicate with
            | t::_ -> Some t
            | _ -> None
    
    member x.Find predicate =
        match x.TryFind predicate with
            | Some t -> t
            | None -> failwith "Not found."

    member x.FilterChildren predicate =
        let rec filter accum left = function
            | [] -> accum
            | x::xs ->
                let n = [ if (predicate x.value) then 
                            yield tree (L (x, Node (x.value, left, p, xs))) ]
                    
                filter (n @ accum) (x::left) xs            

        filter [] [] t.children 

    member x.IsLeaf =
        t.children |> List.isEmpty

    member x.HasChildren =
        x.IsLeaf |> not

    member x.Root =
        match x.Parent with
            | None -> x
            | Some p -> p.Root

    member x.Insert value =
        let n = { value = value; children = [] }
        tree (L (n, Node (value, [], p, t.children)))

    member x.Update value =
        let n = { t with value = value }

        match p with
            | Top _ -> 
                tree (L (n, Top value))
            | Node (_, left, up, right) ->
                tree (L (n, Node (value, left, up, right)));

    member x.Count =
        let rec cnt t =
            t.children |> List.fold (fun c t -> c + cnt t) 1

        cnt t

    member x.BranchingFactor =
        let rec cnt t =
            match t.children with
                | [] -> (0, 0)
                | c -> c |> List.fold (fun (n, b) t ->
                                        let (i, j) = cnt t in (n + i, b + j)
                                       ) (1, List.length t.children)

        let (n, b) = cnt t in float b / float n

    member x.ToJson (f : 'a -> (string * string) list) =

        let printProperty x = sprintf @"""%s"" : ""%s""" (fst x) (snd x)

        let rec printProperties = function
            | x::xs -> printProperty x + ", " + printProperties xs
            | [] -> ""

        let rec printNodes = function
            | x::[] -> printNode x
            | x::xs -> printNode x + ", " + printNodes xs
            | [] -> ""

        and printNode t =
            let props = f t.value
            sprintf @"{ %s ""children"" : [ %s ] }" (printProperties props) (printNodes t.children)
            
        printNode t
            

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Tree =

    let single value = tree<_>.Single value

    let value (tree : tree<'a>) = tree.Value

    let isRoot (tree : tree<'a>) = tree.IsRoot

    let left (tree : tree<'a>) = tree.Left

    let right (tree : tree<'a>) = tree.Right

    let parent (tree : tree<'a>) = tree.Parent

    let child (tree : tree<'a>) = tree.Child

    let children (tree : tree<'a>) = tree.Children

    let filter (predicate : 'a -> bool) (tree : tree<'a>) = tree.Filter predicate

    let tryFind (predicate : 'a -> bool) (tree : tree<'a>) = tree.TryFind predicate

    let find (predicate : 'a -> bool) (tree : tree<'a>) = tree.Find predicate
   
    let filterChildren (predicate : 'a -> bool) (tree : tree<'a>) = tree.FilterChildren predicate

    let hasChildren (tree : tree<'a>) = tree.HasChildren

    let isLeaf (tree : tree<'a>) = tree.IsLeaf

    let root (tree : tree<'a>) = tree.Root

    let insert (value : 'a) (tree : tree<'a>) = tree.Insert value

    let update (value : 'a) (tree : tree<'a>) = tree.Update value

    let count (tree : tree<'a>) = tree.Count

    let branchingFactor (tree : tree<'a>) = tree.BranchingFactor

    let toJson (f : 'a -> (string * string) list) (tree : tree<'a>) = tree.ToJson f