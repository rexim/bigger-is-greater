
let rec take_while f xs = 
  match xs with
  | [] -> []
  | x :: rest -> if f x
                 then x :: take_while f rest
                 else []

let rec drop_while f xs = 
  match xs with
  | [] -> []
  | x :: rest -> if f x
                 then drop_while f rest
                 else xs

let span f xs = (take_while f xs, drop_while f xs)

let string_to_char_list s =
  let rec string_to_char_list_impl i l =
    if i < 0 
    then l 
    else string_to_char_list_impl (i - 1) (s.[i] :: l) in
  string_to_char_list_impl (String.length s - 1) []

let char_list_to_string cs = 
  String.concat "" (List.map (fun x -> String.make 1 x) cs)

let insert_revert x0 rest1 =
  let (left, right) = span (fun x -> x > x0) rest1 in
  match (List.rev left, List.rev right) with
  | ([], revRight) -> List.concat [revRight; [x0]]
  | (l :: revLeft, revRight) -> List.concat [[l]; revRight; [x0]; revLeft]
                
let next_permutation word = 
  let rec next_permutation_impl char_list =
    match char_list with
    | x0 :: rest0 -> 
       (match next_permutation_impl rest0 with
        | (false, x1 :: rest1) when x0 < x1 -> (true, insert_revert x0 (x1 :: rest1))
        | (flag, rest1) -> (flag, x0 :: rest1))
    | [] -> (false, [])
  in
  let char_list = string_to_char_list word in
  match next_permutation_impl char_list with
  | (true, result) -> Some (char_list_to_string result)
  | _ -> None

let _ = 
  let n = read_int () in
  for i = 1 to n do
    let word = read_line () in
    match next_permutation word with
    | Some result -> print_string result;
                     print_string "\n";
    | None -> print_string "no answer\n";
  done
