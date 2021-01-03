let read_input filename =
    let chan = open_in filename in
    let timestamp = input_line chan and
    buses = input_line chan in
    close_in chan;
    let timestamp = int_of_string timestamp and
    buses = buses
        |> String.trim
        |> String.split_on_char ','
        |> List.map int_of_string_opt
        |> List.filter (fun x -> match x with
            | Some _ -> true
            | None -> false)
        |> List.map (fun x -> match x with
            | Some n -> n
            | _ -> failwith "...")
    in
    (timestamp, buses)

let get_earliest timestamp buses =
    let get_wait_time bus_id =
        bus_id - (timestamp mod bus_id)
    in
    let bus_times = buses
        |> List.map (fun bus_id -> (bus_id, get_wait_time bus_id))
    in
    List.fold_left (fun (bus_id_a, wait_time_a) (bus_id_b, wait_time_b) ->
        if wait_time_b < wait_time_a then (bus_id_b, wait_time_b)
        else (bus_id_a, wait_time_a))
        (List.nth bus_times 0)
        (List.init ((List.length bus_times) - 1) (fun idx -> List.nth bus_times (idx + 1)))

let () =
    let timestamp, buses = read_input "input" in
    let bus_id, wait_time = get_earliest timestamp buses in
    Printf.printf "%d\n" (bus_id * wait_time)
