module Action = struct
    type t =
        | North
        | South
        | East
        | West
        | Left
        | Right
        | Forward

    let of_char = function
        | 'N' -> North
        | 'S' -> South
        | 'E' -> East
        | 'W' -> West
        | 'L' -> Left
        | 'R' -> Right
        | 'F' -> Forward
        | _ -> failwith "..."

    let to_string = function
        | North -> "N"
        | South -> "S"
        | East -> "E"
        | West -> "W"
        | Left -> "L"
        | Right -> "R"
        | Forward -> "F"
end

module State = struct
    type t = {
        x : int;
        y : int
    }

    (* state_s -> Ship, state_w -> Waypoint *)
    let move action dis state_w state_s =
        let rotate pt rel_pt deg =
            let rad = (float_of_int deg) *. Float.pi /. 180. in
            let s = sin rad and c = cos rad
            and xf = float_of_int pt.x and yf = float_of_int pt.y in
            let xnew = xf *. c +. yf *. s and
            ynew = -.xf *. s +. yf *. c in
            (xnew |> Float.round |> int_of_float, ynew |> Float.round |> int_of_float)
        in
        match action with
        | Action.North -> { x=state_w.x; y=state_w.y + dis }
        | Action.South -> { x=state_w.x; y=state_w.y - dis }
        | Action.East -> { x=state_w.x + dis; y=state_w.y }
        | Action.West -> { x=state_w.x - dis; y=state_w.y }
        | Action.Left ->
            let xnew, ynew = rotate state_w state_s (-dis) in
            { x=xnew; y=ynew }
        | Action.Right ->
            let xnew, ynew = rotate state_w state_s dis in
            { x=xnew; y=ynew }
        | Action.Forward ->
            { x=state_s.x + state_w.x * dis;  y=state_s.y + state_w.y * dis }

    let init x y = { x=x; y=y }

    let to_string state =
        let x_dir = if (state.x >= 0) then "East" else "West" and
        y_dir = if (state.y >= 0) then "North" else "South" and
        x_dis = abs state.x and
        y_dis = abs state.y in
        (Int.to_string y_dis) ^ y_dir ^ "," ^ (Int.to_string x_dis) ^ x_dir
end

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

let parse_action s =
    let dir = String.get s 0 |> Action.of_char and
    dis = String.sub s 1 (String.length s - 1) |> int_of_string in
    (dir, dis)

let rec run ~state_s ~state_w = function
    | [] -> (state_s, state_w)
    | (dir,dis)::xs ->
        Printf.printf "State : Ship: %s, Waypoint: %s\n" (State.to_string state_s) (State.to_string state_w);
        Printf.printf "Move : %s%4d\n" (Action.to_string dir) dis;
        match dir with
        | Action.Forward -> run ~state_s:(State.move dir dis state_w state_s) ~state_w:state_w xs
        | _ -> run ~state_s:state_s ~state_w:(State.move dir dis state_w state_s) xs

let () =
    let moves = read_input "input"
    |> List.map parse_action
    in
    let state_s = State.init 0 0 and
    state_w = State.init 10 1 in
    let end_state_s, end_state_w = run ~state_s:state_s ~state_w:state_w moves in
    Printf.printf "State : Ship: %s, " (State.to_string end_state_s);
    Printf.printf "Waypoint: %s\n" (State.to_string end_state_w);
    Printf.printf "%d\n" ((end_state_s.x |> abs) + (end_state_s.y |> abs))
