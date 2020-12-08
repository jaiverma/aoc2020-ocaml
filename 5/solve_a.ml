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

let parse_bsp p lo hi lo_i hi_i =
    let rec parse_bsp_impl p lo hi =
        match p with
        | [] -> failwith "..."
        | x::[] -> if x=lo_i then lo else hi
        | x::xs ->
            if x=lo_i then parse_bsp_impl xs lo (hi - (hi - lo + 1) / 2)
            else parse_bsp_impl xs (lo + (hi - lo + 1) / 2) hi
    in
    parse_bsp_impl p lo hi

let get_row p = parse_bsp p 0 127 'F' 'B'

let get_col p = parse_bsp p 0 7 'L' 'R'

let get_seat_id row col = row * 8 + col

let () =
    let seat_ids = read_input "input" |>
    List.map (fun x ->
        let row_pattern = List.init 7 (String.get x) in
        let row = get_row row_pattern and
        col_pattern = List.init 3 (String.get (String.sub x 7 3)) in
        let col = get_col col_pattern in
        (* Printf.printf "row=%d, col=%d\n" row col; *)
        (row, col)
    ) |>
    List.map (fun x ->
        let r,c = x in
        get_seat_id r c
    ) in
    List.fold_left (fun a b -> if a>b then a else b) (List.nth seat_ids 0)
        seat_ids |>
    print_int;
    (* List.iter (fun x -> print_int x; print_newline ()) seat_ids; *)
    print_newline ()
