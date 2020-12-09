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
                    List.map (fun x -> (List.nth x 1) ^ (List.nth x 2)) in
                let head = String.split_on_char ' ' k in
                let head = (List.nth head 0) ^ (List.nth head 1) in
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
            | v::ks -> begin
                List.iter (fun k ->
                    match (Hashtbl.find_opt d k) with
                    | None -> Hashtbl.add d k [v]
                    | Some l -> Hashtbl.replace d k (v :: l)
                ) ks;
                parse_input_impl xs
            end
    in
    parse_input_impl xs;
    d

let find_bags colour d =
    let rec find_bags_impl colour checked =
        match (List.find_opt (fun x -> x=colour) checked) with
        | Some _ -> []
        | None ->
            let value = Hashtbl.find_opt d colour in
            match value with
            | None -> []
            | Some vs ->
                let ret = List.map (fun c -> find_bags_impl c
                    (colour :: checked)) vs in
                vs @ (List.flatten ret)
    in
    find_bags_impl colour []

let () =
    let d = read_input "input" |>
    parse_input in
    (* Hashtbl.iter (fun k v -> Printf.printf "%s\n" k;
        List.iter (fun x -> Printf.printf "\t%s\n" x) v) d; *)
    let set_of_colours = find_bags "shinygold" d |>
        SSet.of_list in
    (* List.iter (fun x -> Printf.printf "%s\n" x) (SSet.elements set_of_colours); *)
    SSet.fold (fun _ n -> n + 1) set_of_colours 0 |>
    print_int;
    print_newline ()
