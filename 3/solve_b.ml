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

let find_path map slope_x slope_y =
    let rec find_path_impl i j n_trees =
        if i>=(Array.length map) then n_trees
        else
            let next_i = i + slope_y and
            next_j = (j + slope_x) mod (Array.length map.(i)) in
            find_path_impl next_i next_j (if map.(i).(j)='#' then (n_trees + 1)
                else n_trees)
    in
    find_path_impl 0 0 0

let () =
    let l = read_input "input" in
    let a1 = find_path l 1 1 and
    a2 = find_path l 3 1 and
    a3 = find_path l 5 1 and
    a4 = find_path l 7 1 and
    a5 = find_path l 1 2 in
    a1 * a2 * a3 * a4 * a5 |>
    print_int;
    print_newline ()
