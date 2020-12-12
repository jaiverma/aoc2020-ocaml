let read_input filename =
    let chan = open_in filename in
    let rec read_lines chan =
        try let line = input_line chan in
            int_of_string (line) :: read_lines chan
        with
            End_of_file -> []
    in
    let ret = read_lines chan in
    close_in chan;
    ret

let verify preamble n =
    let rec sums_to n = function
        | [] -> false
        | [_] -> false 
        | (x::xs) as l ->
            let lo = x and
            hi = match (List.rev xs) with
                | [] -> failwith "..."
                | x'::_ -> x'
            in
            if (lo + hi)=n then true
            else if (lo + hi) < n then sums_to n xs
            else
                match (List.rev l) with
                | [] -> false
                | _::xs' -> sums_to n (List.rev xs')
    in
    sums_to n (List.sort compare preamble)

let get_first_n start n xs =
    let rec get_first_n_impl cur n = function
        | [] ->
            if n=0 then []
            else begin
                Printf.printf "n=%d\n" n;
                failwith "..."
            end
        | x::xs ->
            if n=0 then []
            else if cur>=start then x :: get_first_n_impl (cur + 1) (n - 1) xs
            else get_first_n_impl (cur + 1) n xs
    in
    get_first_n_impl 0 n xs

let run xs n =
    let rec run_impl start xs =
        if (List.length xs - start < n + 1) then failwith "no outlier found..."
        else
            let (preamble, num) =
                let l = get_first_n start (n + 1) xs in
                match (List.rev l) with
                | num::preamble_rev -> (List.rev preamble_rev, num)
                | _ -> failwith "..."
            in
            let ret = verify preamble num in
            if ret then run_impl (start + 1) xs
            else num
    in
    run_impl 0 xs

let contig xs target =
    let sum xs = List.fold_left (+) 0 xs in
    let rec contig_impl acc = function
    | [] -> failwith "..."
    | (x::xs) as l ->
        let sum_acc = sum acc in
        if sum_acc=target then acc
        else if sum_acc < target then contig_impl (acc @ [x]) xs
        else match acc with
            | [] -> failwith "..."
            | _::xs' -> contig_impl xs' l
    in
    contig_impl [] xs

let () =
    let lines = read_input "input" in
    let target = run lines 25 in
    let l = contig lines target in
    let min = List.fold_left (fun init x -> if x>init then x else init) (List.nth l 0) l and
    max = List.fold_left (fun init x -> if x<init then x else init) (List.nth l 0) l in
    Printf.printf "%d\n" (min + max);
