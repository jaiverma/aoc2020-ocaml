let parse_line l =
    let toks = String.split_on_char ' ' l in
    let range, chr, password =
        match toks with
        | a::b::c::[] -> a,b,c
        | _ -> failwith "..."
    in
    let range =
        String.split_on_char '-' range |>
        List.map int_of_string
    in
    let chr =
        match (String.split_on_char ':' chr) with
        | a::b::[] -> a
        | _ -> failwith "..."
    in
    let chr = String.get chr 0
    in
    range, chr, password

let is_valid range chr password =
    let rec is_valid_impl xs freq =
        match xs with
        | [] -> freq
        | x::xs' ->
            if x=chr then is_valid_impl xs' (freq + 1)
            else is_valid_impl xs' freq
    in
    let password_l = List.init (String.length password) (String.get password) in
    let freq = is_valid_impl password_l 0 in
    let lo, hi = match range with
        | a::b::[] -> a, b
        | _ -> failwith "..."
    in
    if freq >= lo && freq <= hi then true
    else false

let read_input filename =
    let chan = open_in filename in
    let rec read_line i =
        try let line = input_line i in
            parse_line line :: read_line i
        with
            End_of_file -> []
    in
    let ret = read_line chan in
    close_in chan;
    ret

let () =
    let l = read_input "input" in
    let ans = List.map (fun x ->
        let range,chr,password = x in
        let ret = is_valid range chr password in
        if ret then 1
        else 0
    ) l in
    let ans = List.fold_left (+) 0 ans in
    print_int ans;
    print_newline ()

