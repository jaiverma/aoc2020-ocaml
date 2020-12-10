module SSet = Set.Make(
    struct
        let compare = compare
        type t = string
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

let rec tokenize s =
    let toks = Str.split (Str.regexp "contain") s in
    let colours = match toks with
    | k::v::[] -> begin
        (* if v contains 'no other bags' then return empty list, otherwise
           split on ',' to get number of colours *)
        try let _ = Str.search_forward (Str.regexp "no other bags") v 0 in
            []
        with
            Not_found ->
                let tail = String.split_on_char ',' v |>
                    List.map String.trim |>
                    List.map (String.split_on_char ' ') |>
                    List.map (fun x -> ((List.nth x 0) |> int_of_string,
                        (List.nth x 1) ^ (List.nth x 2))) in
                let head = String.split_on_char ' ' k in
                let head = (1, (List.nth head 0) ^ (List.nth head 1)) in
                head :: tail
        end
    | _ -> failwith "..."
    in
    colours

let parse_input xs =
    let d = Hashtbl.create 1000 in
    let rec parse_input_impl = function
        | [] -> ()
        | x::xs ->
            let toks = tokenize x in
            match toks with
            | [] -> parse_input_impl xs
            | k::vs -> begin
                List.iter (fun v ->
                    match (Hashtbl.find_opt d k) with
                    | None -> Hashtbl.add d k [v]
                    | Some l -> Hashtbl.replace d k (v :: l)
                ) vs;
                parse_input_impl xs
            end
    in
    parse_input_impl xs;
    d

let find_bags colour d =
    let rec find_bags_impl colour =
        let bags = Hashtbl.find_opt d (1,colour) in
        match bags with
        | None -> 0
        | Some xs ->
            (* xs is a list of bags like so: [(2,red);(5,blue)]
               so 2 * find_bags_impl red + 5 * find_bags_impl blue *)
            match xs with
            | [] -> failwith "..."
            | x::xs' ->
                let n,c = x in
                let n_bags = n + n * find_bags_impl c in
                let op = List.map (fun b ->
                    let n,c=b in
                    n + n * find_bags_impl c) in
                n_bags + List.fold_left (+) 0 (op xs')
    in
    find_bags_impl colour

let () =
    let d = read_input "input" |>
    parse_input in
    (* Hashtbl.iter (fun k v -> Printf.printf "%s\n" k;
        List.iter (fun x -> Printf.printf "\t%s\n" x) v) d; *)
    find_bags "shinygold" d |>
    print_int;
    print_newline ()
