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

module Cell = struct
    type t =
        | Empty
        | Occupied
        | Floor

    let of_char = function
        | 'L' -> Empty
        | '#' -> Occupied
        | '.' -> Floor
        | _ -> failwith "..."

    let is_occupied = function
        | Empty | Floor -> false
        | Occupied -> true

    let to_string = function
        | Empty -> 'L'
        | Occupied -> '#'
        | Floor -> '.'
end

let parse_input xs =
    let grid = Array.init (List.length xs) (fun x ->
        let row = List.nth xs x in
        Array.init (String.length row) (String.get row)
        |> Array.map (fun x -> Cell.of_char x))
    in
    grid

let find_seat grid r c dr dc =
    let rn = Array.length grid and
    cn = Array.length grid.(r) in
    let rec find_seat_impl r c =
        let x = r + dr and
        y = c + dc
        in
        if (x >= 0 && x < rn && y >= 0 && y < cn) then
            match grid.(x).(y) with
            | Cell.Empty -> Cell.Empty
            | Cell.Occupied -> Cell.Occupied
            | Cell.Floor -> find_seat_impl x y
        else
            Floor
    in
    find_seat_impl r c

let neighbours grid r c =
    [
        find_seat grid r c 0 1;
        find_seat grid r c 1 1;
        find_seat grid r c 1 0;
        find_seat grid r c 1 (-1);
        find_seat grid r c 0 (-1);
        find_seat grid r c (-1) (-1);
        find_seat grid r c (-1) 0;
        find_seat grid r c (-1) 1
    ]

let run_round grid =
    let new_grid = Array.map Array.copy grid in
    let xn = Array.length grid and
    yn = (Array.get grid 0) |> Array.length in
    for x=0 to (xn - 1) do
        for y=0 to (yn - 1) do
            let ns = neighbours grid x y in
            let num_occupied = List.filter Cell.is_occupied ns
                |> List.length in
            match grid.(x).(y) with
            | Cell.Occupied ->
                if (num_occupied >= 5) then new_grid.(x).(y) <- Cell.Empty
                else ()
            | Cell.Empty ->
                if (num_occupied=0) then new_grid.(x).(y) <- Cell.Occupied
                else ()
            | _ -> ()
        done;
    done;
    new_grid

let rec run grid =
    let new_grid = run_round grid in
    if (new_grid=grid) then Array.fold_left (fun init x ->
        ((List.filter Cell.is_occupied (Array.to_list x)) |> List.length) + init) 0 grid
    else run new_grid

let print_grid grid =
    let _ = Array.map (fun x ->
        Array.iter (fun y -> Printf.printf "%c " (Cell.to_string y)) x;
        Printf.printf "\n";) grid in
    ()

let () =
    read_input "input"
    |> parse_input
    |> run
    |> Printf.printf "%d\n"
