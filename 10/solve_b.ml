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

let print_bin n cols =
    let rec print_bin_impl n cols acc =
        if n=0 then
            if cols<=0 then acc
            else print_bin_impl n (cols - 1) (0 :: acc)
        else
            let x = n land 1 in
            print_bin_impl (n lsr 1) (cols - 1) (x :: acc)
    in
    let rec format_bin xs =
        match xs with
        | [] -> ""
        | x::xs' -> string_of_int x ^ format_bin xs'
    in
    print_bin_impl n cols []
    |> format_bin


let range start ending =
    List.init (ending - start) (fun x -> x + start)

let consecutive_zero xs =
    let rec f xs cur max =
        match xs with
        | [] -> if cur > max then cur else max
        | x::xs' ->
            if x=0 then f xs' (cur + 1) max
            else
                let new_max = if cur > max then cur else max in
                f xs' 0 new_max
    in
    f xs 0 0

let rec count_ones freq = function
    | [] -> []
    | x::xs ->
        if x=1 then count_ones (freq + 1) xs
        else freq :: count_ones 0 xs

let () =
    let data = read_input "input" in
    let max = List.fold_left (fun init n -> if n > init then n else init) (List.nth data 0) data in
    let data' = 0 :: (max + 3) :: data in
    calc_diff data'
    |> count_ones 0
    |> List.fold_left (fun init n ->
        let x = match (n - 1) with
            | 0 -> 1
            | 1 -> 2
            | 2 -> 4
            | 3 -> 7
            | _ -> 1
        in
        x * init) 1
    |> print_int;
    print_newline ()
