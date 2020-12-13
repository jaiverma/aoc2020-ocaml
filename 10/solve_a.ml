let read_input filename =
    let chan = open_in filename in
    let rec read_lines chan =
        try let line = input_line chan in
            int_of_string line :: read_lines chan
        with
            End_of_file -> []
    in
    let ret = read_lines chan in
    close_in chan;
    ret

let calc_diff xs =
    let xs' = List.sort compare xs in
    let rec calc_diff_impl = function
        | [] -> []
        | [_] -> []
        | x::(y::_ as t) -> y - x :: calc_diff_impl t
    in
    calc_diff_impl xs'

let () =
    let data = read_input "input" in
    let max = List.fold_left (fun init n -> if n > init then n else init) (List.nth data 0) data in
    let data' = 0 :: (max + 3) :: data in
    let diff = calc_diff data' in
    let find_x x = List.filter (fun x' -> x'=x) in
    let ones = find_x 1 diff and
    threes = find_x 3 diff in
    Printf.printf "%d\n" ((List.length ones) * (List.length threes));
