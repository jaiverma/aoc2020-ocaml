let read_input filename =
    let chan = open_in filename in
    let rec read_line i =
        try let line = input_line i in
            int_of_string line :: read_line i
        with
            End_of_file -> []
    in
    let ret = read_line chan in
    close_in chan;
    ret

let solve l num =
    let l_sorted = List.sort compare l in
    let rec first_last l =
        match l with
        | [] -> failwith "..."
        | [e] -> failwith "..."
        | [e1;e2] -> (e1, e2)
        | e1::_::r -> first_last (e1 :: r)
    in
    let rec solve_impl xs =
        let first, last = first_last xs in
        if (first + last) = num then (first, last)
        else if (first + last) < num then
            match xs with
            | [] -> failwith "..."
            | h::t -> solve_impl t
        else
            match (List.rev xs) with
            | [] -> failwith "..."
            | h::t -> solve_impl (List.rev t)
    in
    solve_impl l_sorted

let () =
    let l = read_input "input" in
    let ans = solve l 2020 in
    let a,b = ans in
    print_int (a * b);
    print_newline ()
