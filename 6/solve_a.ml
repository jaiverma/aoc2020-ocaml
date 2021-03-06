module CSet = Set.Make(
    struct
        let compare = compare
        type t = char
    end)

let read_input filename =
    let chan = open_in filename in
    let rec read_lines i =
        try let line = input_line i in
            line :: read_lines i
        with
            End_of_file -> []
     in
     let ret = read_lines chan in
     close_in chan;
     ret

let parse_input xs =
    let rec parse_input_impl acc = function
        | [] -> [acc]
        | x::xs ->
            if x="" then acc :: parse_input_impl [] xs
            else parse_input_impl (x :: acc) xs
    in
    let tcs = parse_input_impl [] xs in
    let rec make_list = function
        | [] -> []
        | x::xs ->
            let l = List.init (String.length x) (String.get x) in
            l @ make_list xs
    in
    List.map make_list tcs

let unique_ans t =
    let s = CSet.of_list t in
    CSet.fold (fun _ n -> n + 1) s 0

let () =
    read_input "input" |>
    parse_input |>
    List.map (fun x -> unique_ans x) |>
    List.fold_left (fun a b -> a + b) 0 |>
    print_int;
    print_newline ()
