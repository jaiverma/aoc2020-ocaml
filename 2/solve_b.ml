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
    let lo,hi = match range with
    | lo::hi::[] -> lo,hi
    | _ -> failwith "..."
    in
    let lo_c = String.get password (lo - 1) in
    let hi_c = String.get password (hi - 1) in
    let total = List.fold_left (+) 0 [if lo_c=chr then 1 else 0;
        if hi_c=chr then 1 else 0] in
    total=1

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

