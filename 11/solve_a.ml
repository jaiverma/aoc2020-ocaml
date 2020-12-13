let read_input filename =
    let chan = open_in filename in
    let rec read_lines chan =
        try let line = input_line chan in
            line :: read_lines chan
        with
            End_of_file -> []
    in
    let ret = read_lines chan in
    close_in chan;
    ret

let parse_input xs =
    let map = Array.init (List.length xs) (fun x ->
        let row = List.nth xs x in
        Array.init (String.length row) (String.get row))
    in
    map

let cell map x y =
    Array.get map x
    |> Array.get
    |> fun map_x -> map_x y

let occupy map x y = map.(x).(y) <- '#'; ()

let vacate map x y = map.(x).(y) <- 'L'; ()

(* return list of valid neighbours *)
let neighbours map x y =
    let acc = ref [] in
    (* x + 1, y *)
    if (x + 1) < (Array.length map) then
        acc := (x + 1, y) :: !acc;
    (* x + 1, y - 1 *)
    if (x + 1) < (Array.length map) && (y - 1 >= 0) then
        acc := (x + 1, y - 1) :: !acc;
    (* x, y - 1 *)
    if (y - 1) >= 0 then
        acc := (x, y - 1) :: !acc;
    (* x - 1, y - 1 *)
    if (x - 1) >= 0 && (y - 1) >= 0 then
        acc := (x - 1, y - 1) :: !acc;
    (* x - 1, y *)
    if (x - 1) >= 0 then
        acc := (x - 1, y) :: !acc;
    (* x - 1, y + 1 *)
    if (x - 1) >= 0 && (y + 1) < (Array.get map x |> Array.length) then
        acc := (x - 1, y + 1) :: !acc;
    (* x, y + 1 *)
    if (y + 1) < (Array.get map x |> Array.length) then
        acc := (x, y + 1) :: !acc;
    (* x + 1, y + 1 *)
    if (x + 1) < (Array.length map) && (y + 1) < (Array.get map x |> Array.length) then
        acc := (x + 1, y + 1) :: !acc;
    !acc

(* updates seat at (x,y), Arrays are mutable so no new array is returned *)
let update orig_map map x y =
    let ns = neighbours orig_map x y in
    let n_occupied = List.length (List.filter (fun (x,y) -> (cell orig_map x y)='#') ns) and
    n_empty = List.length (List.filter (fun (x,y) -> (cell orig_map x y)='L') ns) and
    n_seats = List.length (List.filter (fun (x,y) ->
        let seat = cell orig_map x y in
        seat='L' || seat='#') ns) in
    let _ = match (cell orig_map x y) with
    | 'L' ->
        if n_empty = n_seats then occupy map x y;
    | '#' ->
        if n_occupied >= 4 then vacate map x y;
    | _ -> ()
    in
    ()

let print_map map =
    let _ = Array.map (fun x ->
        Array.iter (fun y -> Printf.printf "%c " y) x;
        Printf.printf "\n";) map in
    ()

let () =
    let rec run map =
        let old_map = Array.map Array.copy map in
        let xn = Array.length map and
        yn = (Array.get map 0) |> Array.length in
        for x=0 to (xn - 1) do
            for y=0 to (yn - 1) do
                update old_map map x y
            done;
        done;
        (* Printf.printf "before:\n";
        print_map old_map;
        Printf.printf "after:\n";
        print_map map;
        Printf.printf "---------------\n"; *)
        if old_map=map then Array.fold_left (fun init x ->
            ((List.filter (fun e -> e='#') (Array.to_list x))
            |> List.length) + init) 0 map
        else run map
    in
    read_input "input"
    |> parse_input
    |> run
    |> Printf.printf "%d\n";
