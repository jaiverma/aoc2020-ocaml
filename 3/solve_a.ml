let parse_line l =
    List.init (String.length l) (String.get l)

let read_input filename =
    let chan = open_in filename in
    let rec read_lines i =
        try let line = input_line i in
            parse_line line :: read_lines i
        with
            End_of_file -> []
    in
    let ret = read_lines chan in
    close_in chan;
    List.map (fun x -> Array.of_list x) ret |>
        Array.of_list

let rec find_path map i j n_trees =
    if i=(Array.length map) then n_trees
    else
        let next_i = i + 1 and
        next_j = (j + 3) mod (Array.length map.(i)) in
        find_path map next_i next_j (if map.(i).(j)='#' then (n_trees + 1)
            else n_trees)

let () =
    let l = read_input "input" in
    find_path l 0 0 0 |>
    print_int;
    print_newline ()
