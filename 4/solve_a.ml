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

(* let rec flatten xs =
    match xs with
    | [] -> []
    | x::xs' -> x @ flatten xs' *)

let parse_input lst =
    let records = ref [] in
    let rec parse_impl xs acc =
        match xs with
        | [] -> records := acc :: !records;
        | x::xs' ->
            if x="" then
            begin
                records := acc :: !records;
                parse_impl xs' []
            end
            else parse_impl xs' (x :: acc)
    in
    parse_impl lst [];

    (* check to see if any of the list elements has more than one key-value
       pairs. if so, expand them *)
    let f = List.map (fun x -> String.split_on_char ' ' x) in
    let records = List.map f !records in
    List.map (fun x -> List.flatten x) records

let is_valid record =
    let keys = List.map (fun x ->
        match (String.split_on_char ':' x) with
        | k::v::[] -> k
        | _ -> failwith "...") record
    in
    let rec is_valid_impl lst check =
        match lst with
        | [] -> false
        | h::t ->
            if h=check then true
            else is_valid_impl t check
    in
    let keys' = ["byr";"iyr";"eyr";"hgt";"hcl";"ecl";"pid"] in
    let rec check_all k =
        match k with
        | [] -> true
        | x::xs ->
            if (is_valid_impl keys x) then check_all xs
            else false
    in
    check_all keys'

let () =
    read_input "input" |>
        parse_input |>
        List.filter (fun x -> is_valid x) |>
        List.length |>
        print_int;
    print_newline ()
