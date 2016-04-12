open Zipper

let hum = Branch (6, [leaf 3; leaf 4; Branch (9, [leaf 7])])
let hum2 = Branch (1, [Branch (2, [Branch (3, []); Branch (4, [])]); Branch (5, []); Branch (6, [Branch (7, [Branch (8, [])])])])

let test =
      (hum, Top)
      |> move_down
      |> move_right
      |> move_right
      |> move_down

let _ = insert_left test (leaf 45) |> move_up |> move_up

let test1 = [Some (6, true); Some (3, false); Some(4, false); Some(9, true); Some (7, false); None; None]

let test2 = [Some (1, true); Some(2, true); Some(3, false); Some(4, false);None;Some(5,false);Some(6, true);Some(7,true);Some(8,false);None;None;None]

let t1 = deserialize test1
let t2 = deserialize test2

let list_cmp l1 l2 =
    List.for_all2
    (fun e1 e2 ->
        match e1,e2 with
            | Some(x1,c1),Some(x2,c2) -> x1=x2 && c1=c2
            | None,None -> true
            | Some _,None | None,Some _ -> false
    )
    l1 l2

(*let t3 = (serialize hum == deserialize test1)*)
(*let t4 = (serialize hum2 == deserialize test2)*)

(*let t5 = (serialize hum == test1)*)
(*let t6 = (serialize hum2 == test2)*)
