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
        | dir -> failwith "..."
end

module Direction = struct
    type t =
        | North
        | South
        | East
        | West

    let turn ~cur ~action ~deg =
        let rec index_of x = function
            | [] -> failwith "..."
            | x'::xs -> if x=x' then 0 else 1 + index_of x xs
        in
        let dirs = [North; East; South; West] and
        dirs' = [North; West; South; East] and
        shift = deg / 90 in
        match action with
        | Action.Right ->
            let idx = index_of cur dirs in
            let new_idx = (idx + shift) mod 4 in
            List.nth dirs new_idx
        | Action.Left ->
            let idx = index_of cur dirs' in
            let new_idx = (idx + shift) mod 4 in
            List.nth dirs' new_idx
        | _ -> failwith "..."

    let to_string = function
        | North -> "North"
        | South -> "South"
        | East -> "East"
        | West -> "West"

    let of_char = function
        | 'N' -> North
        | 'E' -> East
        | 'W' -> West
        | 'S' -> South
        | _ -> failwith "..."
end

module State = struct
    type t = {
        x : int;
        y : int;
        face : Direction.t
    }

    let move action dis state =
        match action with
        | Action.North -> { x=state.x; y=state.y + dis; face=state.face }
        | Action.South -> { x=state.x; y=state.y - dis; face=state.face }
        | Action.East -> { x=state.x + dis; y=state.y; face=state.face }
        | Action.West -> { x=state.x - dis; y=state.y; face=state.face }
        | Action.Left | Action.Right -> { x=state.x; y=state.y;
            face=Direction.turn ~cur:state.face ~action:action ~deg:dis }
        | Action.Forward ->
            match state.face with
            | Direction.North -> { x=state.x; y=state.y + dis; face=state.face }
            | Direction.South -> { x=state.x; y=state.y - dis; face=state.face }
            | Direction.East -> { x=state.x + dis; y=state.y; face=state.face }
            | Direction.West -> { x=state.x - dis; y=state.y; face=state.face }

    let to_string state =
        let x_dir = if (state.x >= 0) then Direction.East else Direction.West and
        y_dir = if (state.y >= 0) then Direction.North else Direction.South and
        x_dis = abs state.x and
        y_dis = abs state.y in

        (Int.to_string y_dis) ^ (Direction.to_string y_dir) ^ "," ^
        (Int.to_string x_dis) ^ (Direction.to_string x_dir)

    let init x y face = { x=x; y=y; face=face }
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

let rec run state = function
    | [] -> state
    | (dir,dis)::xs -> run (State.move dir dis state) xs

let () =
    let moves = read_input "input"
    |> List.map parse_action
    in
    let state = State.init 0 0 Direction.East in
    let end_state = run state moves in
    Printf.printf "%s\n" (State.to_string end_state);
    Printf.printf "%d\n" ((end_state.x |> abs) + (end_state.y |> abs))
