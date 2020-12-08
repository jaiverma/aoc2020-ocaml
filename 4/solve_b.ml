type passport = {
    byr: string option;
    iyr: string option;
    eyr: string option;
    hgt: string option;
    hcl: string option;
    ecl: string option;
    pid: string option;
    cid: string option;
}

let is_yr_valid yr lo hi =
    if yr >= lo && yr <= hi then true
    else false

let is_byr_valid p =
    match p.byr with
    | None -> false
    | Some byr ->
        if (String.length byr) != 4 then false
        else
            match (int_of_string_opt byr) with
            | None -> false
            | Some x -> is_yr_valid x 1920 2002

let is_iyr_valid p =
    match p.iyr with
    | None -> false
    | Some iyr ->
        if (String.length iyr) != 4 then false
        else
            match (int_of_string_opt iyr) with
            | None -> false
            | Some x -> is_yr_valid x 2010 2020

let is_eyr_valid p =
    match p.eyr with
    | None -> false
    | Some eyr ->
        if (String.length eyr) != 4 then false
        else
            match (int_of_string_opt eyr) with
            | None -> false
            | Some x -> is_yr_valid x 2020 2030

let is_hgt_valid p =
    match p.hgt with
    | None -> false
    | Some hgt ->
        let suffix = String.sub hgt (String.length hgt - 2) 2 in
        if suffix <> "cm" && suffix <> "in" then false
        else
            let prefix = String.init (String.length hgt - 2) (String.get hgt) in
            match (int_of_string_opt prefix) with
            | None -> false
            | Some x ->
                if suffix="cm" then (x >= 150 && x <= 193)
                else (x >= 59 && x <= 76)

let is_hcl_valid p =
    match p.hcl with
    | None -> false
    | Some hcl ->
        if (String.length hcl) != 7 then false
        else
            let prefix = String.get hcl 0 in
            if prefix != '#' then false
            else match (int_of_string_opt ("0x" ^ String.sub hcl 1 6)) with
            | None -> false
            | Some _ -> true

let is_ecl_valid p =
    match p.ecl with
    | None -> false
    | Some ecl -> (ecl="amb" || ecl="blu" || ecl="brn" || ecl="gry" ||
        ecl="grn" || ecl="hzl" || ecl="oth")

let is_pid_valid p =
    match p.pid with
    | None -> false
    | Some pid ->
        if (String.length pid) != 9 then false
        else match (int_of_string_opt pid) with
        | None -> false
        | Some _ -> true

let is_cid_valid (p: passport) = true

let rec get_field k = function
    | [] -> None
    | (k',v)::t ->
        if k=k' then Some v
        else get_field k t

let create_passport record =
    let p: passport = {
        byr=get_field "byr" record;
        iyr=get_field "iyr" record;
        eyr=get_field "eyr" record;
        hgt=get_field "hgt" record;
        hcl=get_field "hcl" record;
        ecl=get_field "ecl" record;
        pid=get_field "pid" record;
        cid=get_field "cid" record;
    } in
    p

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

let parse_input lst =
    let records = ref [] in
    let rec parse_impl xs acc =
        match xs with
        | [] -> records := acc :: !records;
        | x::xs' ->
            if x="" then
            begin
                records := acc :: !records;
                parse_impl xs' []
            end
            else parse_impl xs' (x :: acc)
    in
    parse_impl lst [];

    (* check to see if any of the list elements has more than one key-value
       pairs. if so, expand them *)
    let f = List.map (fun x -> String.split_on_char ' ' x) in
    let records = List.map f !records in
    let records = List.map (fun x -> List.flatten x) records in
    let rec rearrange lst =
        match lst with
        | [] -> []
        | h::t ->
            match (String.split_on_char ':' h) with
            | k::v::[] ->  (k,v) :: rearrange t
            | _ -> failwith "..."
    in
    List.map (fun x -> rearrange x) records

let is_valid p =
    (is_byr_valid p) && (is_iyr_valid p) && (is_eyr_valid p) &&
        (is_hgt_valid p) && (is_hcl_valid p) && (is_ecl_valid p) &&
        (is_pid_valid p) && (is_cid_valid p)

let () =
    read_input "input" |>
        parse_input |>
        List.map (fun x -> create_passport x) |>
        List.filter (fun x -> is_valid x) |>
        List.length |>
        print_int;
    print_newline ()
