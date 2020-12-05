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

let rec combinations l k =
    if k=0 then [[]]
    else
        match l with
        | [] -> []
        | x::xs -> List.map (fun y -> x :: y) (combinations xs (k - 1)) @ combinations xs k

let sum l k n =
    let c = combinations l k in
    List.filter (fun x -> (List.fold_left (+) 0 x) = n) c

let () =
    let l = read_input "input" in
    let ans::[] = sum l 3 2020 in
    let a::b::c::[] = ans in
    print_int (a * b * c);
    print_newline ()


(* let () =
    let l = read_input "input" in
    let ans = sum l 3 2020 in
    List.iter (fun x -> match x with
        | a::b::c::[] ->
            print_int a; print_newline ();
            print_int b; print_newline ();
            print_int c; print_newline ();
        | _ -> failwith "..."
    ) ans *)
