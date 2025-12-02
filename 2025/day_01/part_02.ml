(*
   From the puzzle:
  (* 
    Following the same rotations as in the above example, the dial points at zero a few extra times during its rotations:

    - The dial starts by pointing at 50.
    - The dial is rotated L68 to point at 82; during this rotation, it points at 0 once.
    - The dial is rotated L30 to point at 52.
    - The dial is rotated R48 to point at 0.
  *)
   
   Solving by tracking cumulative position and count crossings

   To count zero crossings between two positions:
     abs((old_pos - 1) / 100 - (new_pos - 1) / 100)

   The (pos - 1) before division handles the edge case where we land exactly on a multiple
   of 100 (like landing on 0, 100, 200, etc). Without the -1, landing on 100 would be
   counted as a crossing, but we count it separately as "landed on zero".

   Edge case: multiple complete rotations
   L444 from cumulative = 50: cumulative = 50 - 444 = -394 (dial shows 6)
     Crossings: (50-1)/100 - (-394-1)/100 = 0 - (-4) = 4
     This counts crossing through 0, -100, -200, -300 (four multiples of 100)
     Each multiple of 100 displays as 0 on the dial, so we see 0 four times

   Edge case: R1000 from cumulative = 50: cumulative = 50 + 1000 = 1050 (dial shows 50)
     Crossings: (50-1)/100 - (1050-1)/100 = 0 - 10 = 10
     We cross: 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000
     All of these display as 0 on the dial

   The key insight: every multiple of 100 displays as 0 on the dial
   The formula counts all such crossings by dividing positions by 100
   The -1 adjustment ensures landing exactly on a multiple of 100 isn't counted twice
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

(* computes floor division, OCaml's / operator does truncating division *)
let floor_div x y =
  let q = x / y in
  let r = x mod y in
  if (r < 0 && y > 0) || (r > 0 && y < 0) then q - 1 else q

let apply_move cumulative_pos { direction; distance } =
  match direction with
  | Direction.Left -> cumulative_pos - distance
  | Direction.Right -> cumulative_pos + distance

let split_at (idx : int) (s : string) =
  (CCString.take idx s, CCString.drop idx s)

let count_zero_hits old_pos new_pos =
  if new_pos >= old_pos then floor_div new_pos 100 - floor_div old_pos 100
  else floor_div (old_pos - 1) 100 - floor_div (new_pos - 1) 100

let main () =
  let cumulative_start = 50 in
  let _, result =
    read_lines "2025/day_01/input.txt"
    |> List.map (fun s -> split_at 1 s)
    |> List.map (fun (left, right) ->
        let direction = Direction.of_string left in
        let distance = int_of_string right in
        { direction; distance })
    |> List.fold_left
         (fun (cumulative_pos, count_zero) combo ->
           let new_cumulative = apply_move cumulative_pos combo in
           let zero_hits = count_zero_hits cumulative_pos new_cumulative in
           let new_count = count_zero + zero_hits in
           (new_cumulative, new_count))
         (cumulative_start, 0)
  in
  Printf.printf "%d\n" result

let () = main ()
