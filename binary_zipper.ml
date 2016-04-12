type 'a tree = E | N of 'a tree * 'a * 'a tree * int

type 'a path =
    | Root
    | Left of 'a * 'a tree * 'a path
    | Right of 'a tree * 'a * 'a path

type 'a zipper = 'a tree * 'a path

(* The zipper contrains as annouced :
- the pointed subtree
- the rest of the tree breaked along the path to the root *)

(*Then we define the pointer movements (one for each pointer in the data*)
(*structure) :*)

exception Top
exception Bottom

(* To be replaced by a balancing constructor *)
let makeDTree = fun l v r -> N (l, v, r, 0)

let move_left = fun (tree, path) ->
match tree with
    | E -> raise Bottom
    | N (l, v, r, _) -> (l, Left (v, r, path))

let move_right = fun (tree, path) ->
match tree with
    | E -> raise Bottom
    | N (l, v, r, _) -> (r, Right (l, v, path))

let move_up = fun (tree, path) ->
match path with
    | Root -> raise Top
    | Left (v, r, tail) -> (makeDTree tree v r, tail)
    | Right (l, v, tail) -> (makeDTree l v tree, tail)

(*Now we can build an arbitrary large tree by the following procedure :
- build a tree of bounded depth
- choose the node which will be developped next
- move the current pointer to that node
- continue building the tree*)

let rec move_to_top = function ((tree, path) as pointer) ->
    match path with
    | Root -> pointer
    | Left (v, r, tail) -> move_to_top (makeDTree tree v r, tail)
    | Right (l, v, tail) -> move_to_top (makeDTree l v tree, tail)

let rec move_to x = function ((tree, path) as pointer) ->
    match tree with
    | E ->
        (match path with
        | Right (_, rv, _) when x <= rv ->
                move_to x (move_up pointer)

        | Left (lv, _, _) when x >= lv ->
                move_to x (move_up pointer)
        | _ -> pointer)

    | N (_, v, _, _) ->
            match compare x v with
            | n when n < 0 ->
                    (match path with
                    | Right (_, rv, _) when x < rv ->
                            move_to x (move_up pointer)
                    | Right _ | Root | Left _ ->
                            move_to x (move_left pointer)
                            )

            | n when n > 0 ->
                    (match path with
                    | Left (lv, _, _) when x > lv ->
                            move_to x (move_up pointer)
                    | Left _ | Root | Right _ ->
                            move_to x (move_right pointer)
                            )
                    | _ -> pointer

let rec member_path x = function
    | Right (l, v, tail) ->
        (match compare x v with
        | n when n < 0 -> member x l
        | 0 -> true
        | _ -> member_path x tail)

    | Left (v, r, tail) ->
        (match compare x v with
        | n when n > 0 -> member x r
        | 0 -> true
        | _ -> member_path x tail)

    | Root -> false

let rec zipper_member x = function (tree, path) ->
    match tree with

    | E -> member_path x path
    | N (l, v, r, _) ->
        match compare x v with
        | n when n < 0 ->
                (match path with
                | Right (_, rv, _) when x <= rv -> member_path x path
                | Right _ | Root | Left _ -> member x l)

        | n when n > 0 ->
                (match path with
                | Left (lv, _, _) when x >= lv -> member_path x path
                | Left _ | Root | Right _ -> member x r)

        | _ -> true

let current_tree = function (tree, _) -> tree

let current_value = function (tree, _) ->
    match tree with
        | E -> None
        | N (_, v, _, _) -> Some v

let current_value' = function (tree, _) ->
    match tree with
        | E -> raise Empty
        | N (_, v, _, _) -> v

let rec zipper_insert x = function ((tree, path) as pointer) ->
    match tree with
    | E ->
        (match path with
        | Right (_, rv, _) when x <= rv ->
                zipper_insert x (move_up pointer)
        | Left (lv, _, _) when x >= lv ->
                zipper_insert x (move_up pointer)
        | _ -> (makeTree E x E, path))

    | N (_, v, _, _) ->
        match compare x v with

        | n when n < 0 ->
                (match path with
                | Right (_, rv, _) when x < rv ->
                        zipper_insert x (move_up pointer)
                | Right _ | Root | Left _ ->
                        zipper_insert x (move_left pointer))

        | n when n > 0 ->
                (match path with
                | Left (lv, _, _) when x > lv ->
                        zipper_insert x (move_up pointer)
                | Left _ | Root | Right _ ->
                        zipper_insert x (move_right pointer))

        | _ -> pointer

let rec zipper_delete x = function ((tree, path) as pointer) ->
    match tree with
    | E ->
        (match path with
        | Right (_, rv, _) when x <= rv ->
                zipper_delete x (move_up pointer)
        | Left (lv, _, _) when x >= lv ->
                zipper_delete x (move_up pointer)
        | _ -> pointer)

    | N (l, v, r, _) ->
        match compare x v with
        | n when n < 0 ->

                (match path with
                | Right (_, rv, _) when x < rv ->
                        zipper_delete x (move_up pointer)
                | Right _ | Root | Left _ ->
                        zipper_delete x (move_left pointer)
                        )

        | n when n > 0 ->

                (match path with
                | Left (lv, _, _) when x > lv ->
                    zipper_delete x (move_up pointer)
                | Left _ | Root | Right _ ->
                    zipper_delete x (move_right pointer))

        | _ -> move_to x (appendB l r, path)

let rec path_to_list result = function
    | Root -> result
    | Left (v, r, path) ->
            path_to_list (result @ v :: to_ordered_list r) path
    | Right (l, v, path) ->
            path_to_list (to_ordered_list_rec (v :: result) l) path

let zipper_to_list = function (tree, path) ->
    path_to_list (to_list tree) path
