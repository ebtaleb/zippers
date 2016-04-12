type 'a btree = L of 'a | N of 'a btree * 'a btree

let load input =
  let rec load stack = function
    | [] -> stack
    | Some elem :: rest -> load (L elem :: stack) rest
    | None :: rest ->
      match stack with
        | right :: left :: stack -> load (N(left, right) :: stack) rest
        | [] | [_] -> failwith "incorrect node arity"
  in
  match load [] input with
    | res :: [] -> res
    | [] -> failwith "no input"
    | _ :: _ :: _ -> failwith "remaining nodes"

let t = load [Some 2; Some 0; None; Some 5; Some 9; Some 20; None; None; None]

let rec preorder f lvl = function
    L v        -> f lvl v
  | N (l,r) ->
                    Printf.printf "lvl %d\n" lvl;
                    preorder f (lvl + 1) l;
                    preorder f (lvl + 1) r

let _ = preorder (fun x y -> Printf.printf "<%d>%d\n" x y) 0 t
