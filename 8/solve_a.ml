type reg_state = {
    acc: int;
    pc: int;
}

let decode insn opnd regs =
    match insn with
    | "acc" -> { acc=regs.acc + opnd; pc=regs.pc + 1 }
    | "nop" -> { acc=regs.acc; pc=regs.pc + 1 }
    | "jmp" -> { acc=regs.acc; pc=regs.pc + opnd }
    | _ -> failwith "..."

module ISet = Set.Make(
    struct
        type t = int
        let compare = compare
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

let tokenize line =
    match (String.trim line |> String.split_on_char ' ') with
    | insn::opnd::[] -> (insn, int_of_string opnd)
    | _ -> failwith "..."

let run (insns: (string * int) list) =
    let state = { acc=0; pc=0; } and
    done_pc = ISet.empty in
    let rec run_impl state done_pc =
        if (ISet.mem state.pc done_pc) then state
        else
            let (insn,opnd) = List.nth insns state.pc in
            let new_state = decode insn opnd state in
            run_impl new_state (ISet.add state.pc done_pc)
    in
    run_impl state done_pc

let () =
    let data = read_input "input" |> List.map tokenize in
    let state = run data in
    Printf.printf "acc=%d\n" state.acc
