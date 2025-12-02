(* 
   From the puzzle:
  (* 
    Following these rotations would cause the dial to move as follows:

    - The dial starts by pointing at 50.
    - The dial is rotated L68 to point at 82.
    - The dial is rotated L30 to point at 52.
    - The dial is rotated R48 to point at 0.
  *)
    Solving using modular arithmetic

    (50 + (-68)) % 100 = 82
    (82 + (-30)) % 100 = 52
    (52 + 48) % 100 = 0
*)

open Containers
open CCIO

let read_lines (filename : string) : string list = with_in filename read_lines_l

module Direction = struct
  type t = Left | Right

  let of_string = function
    | "L" -> Left
    | "R" -> Right
    | _ -> failwith "invalid direction"
end

type combination = { direction : Direction.t; distance : int }

(* computes the modulo, OCaml's mod computes the integer remainder *)
let modulo x y =
  let result = x mod y in
  if result >= 0 then result
  else result + y

let spin_dial acc { direction; distance } =
  match direction with
  | Direction.Left -> modulo (acc - distance) 100
  | Direction.Right -> modulo (acc + distance) 100

let split_at (idx : int) (s : string) =
  (CCString.take idx s, CCString.drop idx s)

let main () =
  let dial_start = 50 in
  let _, result =
    read_lines "2025/day_01/input.txt"
    |> List.map (fun s -> split_at 1 s)
    |> List.map (fun (left, right) ->
        let direction = Direction.of_string left in
        let distance = int_of_string right in
        { direction; distance })
    |> List.fold_left
         (fun (acc, count_zero) combo ->
           let new_acc = spin_dial acc combo in
           let new_count = if new_acc = 0 then count_zero + 1 else count_zero in
           (new_acc, new_count))
         (dial_start, 0)
  in
  Printf.printf "%d\n" result

let () = main ()
