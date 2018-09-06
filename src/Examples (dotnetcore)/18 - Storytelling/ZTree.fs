﻿namespace Aardvark.Base

open System.Collections.Generic

// The inner representation of the tree as nodes
type private ZTreeNode<'a> = {
    value : 'a
    children : ZTreeNode<'a> list
}

// Path type for the zipper
type private ZTreePath<'a> =
    | Top of 'a
    | Node of 'a * ZTreeNode<'a> list * ZTreePath<'a> * ZTreeNode<'a> list

    member x.getValue =
        match x with
            | Top v
            | Node (v, _, _, _) -> v

// Zipper
type private ZTreeLocation<'a> = L of ZTreeNode<'a> * ZTreePath<'a>

// The tree class which is just a wrapper for the
// private zipper
type ZTree<'a> private (location : ZTreeLocation<'a>) =

    let (L(t, p)) = location

    static member Single (value : 'a) =
        let n = { value = value; children = [] }
        ZTree (L (n, Top value))

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
                Some (ZTree (L (l, Node (l.value, left, up, t::right))))

    member x.Right =
        match p with
            | Top _ | Node (_, _, _, []) -> 
                None
            | Node (_, left, up, r::right) -> 
                Some (ZTree (L (r, Node (r.value, t::left, up, right))))

    member x.Parent =
        match p with
            | Top _ -> 
                None
            | Node (_, left, up, right) ->
                Some (ZTree (L ({ value = up.getValue
                                  children = List.rev left @ t::right }, up)
                            )
                     )

    member x.Child =
        match t.children with
            | [] -> None
            | x::xs -> Some (ZTree (L(x, Node(x.value, [], p, xs))))

    member x.Children =
        let rec sib accum (t : ZTree<'a>) =
            match t.Right with
                | None -> t::accum
                | Some r -> sib (t::accum) r

        match x.Child with
            | None -> []
            | Some t -> sib [] t

    member x.Filter (predicate : 'a -> bool) = [
            if predicate t.value then yield x
            yield! x.Children 
                        |> List.collect (fun t -> t.Filter predicate)
        ]

    // Find methods use filter, may be optimized
    member x.TryFind (predicate : 'a -> bool) =
        match x.Filter predicate with
            | t::_ -> Some t
            | _ -> None
    
    member x.Find (predicate : 'a -> bool) =
        match x.TryFind predicate with
            | Some t -> t
            | None -> raise (KeyNotFoundException ())

    member x.FilterChildren (predicate : 'a -> bool) =
        let rec filter accum left = function
            | [] -> accum
            | x::xs ->
                let n = [ if (predicate x.value) then 
                            yield ZTree (L (x, Node (x.value, left, p, xs))) ]
                    
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

    member x.Insert (value : 'a) =
        let n = { value = value; children = [] }
        ZTree (L (n, Node (value, [], p, t.children)))

    member x.Set (value : 'a) =
        let n = { t with value = value }

        match p with
            | Top _ -> 
                ZTree (L (n, Top value))
            | Node (_, left, up, right) ->
                ZTree (L (n, Node (value, left, up, right)));

    member x.Update (f : 'a -> 'a) =
        x.Set (f x.Value)

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
            
and 'a ztree = ZTree<'a>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ZTree =

    let single value = ZTree<_>.Single value

    let value (tree : ZTree<'a>) = tree.Value

    let isRoot (tree : ZTree<'a>) = tree.IsRoot

    let left (tree : ZTree<'a>) = tree.Left

    let right (tree : ZTree<'a>) = tree.Right

    let parent (tree : ZTree<'a>) = tree.Parent

    let child (tree : ZTree<'a>) = tree.Child

    let children (tree : ZTree<'a>) = tree.Children

    let filter (predicate : 'a -> bool) (tree : ZTree<'a>) = tree.Filter predicate

    let tryFind (predicate : 'a -> bool) (tree : ZTree<'a>) = tree.TryFind predicate

    let find (predicate : 'a -> bool) (tree : ZTree<'a>) = tree.Find predicate
   
    let filterChildren (predicate : 'a -> bool) (tree : ZTree<'a>) = tree.FilterChildren predicate

    let hasChildren (tree : ZTree<'a>) = tree.HasChildren

    let isLeaf (tree : ZTree<'a>) = tree.IsLeaf

    let root (tree : ZTree<'a>) = tree.Root

    let insert (value : 'a) (tree : ZTree<'a>) = tree.Insert value

    let set (value : 'a) (tree : ZTree<'a>) = tree.Set value

    let update (f : 'a -> 'a) (tree : ZTree<'a>) = tree.Update f

    let count (tree : ZTree<'a>) = tree.Count

    let branchingFactor (tree : ZTree<'a>) = tree.BranchingFactor

    let toJson (f : 'a -> (string * string) list) (tree : ZTree<'a>) = tree.ToJson f