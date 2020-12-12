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
    let rec run_impl state done_pc insn_stack =
        let (insn,opnd) = List.nth insns state.pc in
        let new_state = decode insn opnd state in
        (* check if new state pc has been executed before,
           if it has, return the old state *)
        if (ISet.mem new_state.pc done_pc) then insn_stack
        else run_impl new_state (ISet.add state.pc done_pc) (state.pc :: insn_stack)
    in
    run_impl state done_pc []

let runs_to_end insns =
    let state = { acc=0; pc=0; } and
    done_pc = ISet.empty in
    let rec runs_to_end_impl state done_pc =
        if state.pc >= (List.length insns) then (true,state)
        else
            let (insn,opnd) = List.nth insns state.pc in
            let new_state = decode insn opnd state in
            if (ISet.mem new_state.pc done_pc) then (false,state)
            else runs_to_end_impl new_state (ISet.add state.pc done_pc)
    in
    runs_to_end_impl state done_pc

let replace_insn insns idx =
    let rec replace_insn_impl insns cur =
        match insns with
        | [] -> []
        | x::xs ->
            if cur=idx then
                let insn,opnd = x in
                let new_insn = match insn with
                    | "nop" -> "jmp"
                    | "jmp" -> "nop"
                    | _ -> insn
                in
                (new_insn,opnd) :: replace_insn_impl xs (cur + 1)
            else
                x :: replace_insn_impl xs (cur + 1)
    in
    replace_insn_impl insns 0


let () =
    let data = read_input "input" |> List.map tokenize in
    (* in state we have the instruction which was executed a second time
       so try changing the instruction which caused execution to jump here *)
    let pc_stack = run data in
    let rec run_all insns = function
        | [] -> failwith "..."
        | x::xs ->
            let new_insns = replace_insn insns x in
            let (ans,state) = runs_to_end new_insns in
            if ans then (new_insns,state)
            else run_all insns xs
    in
    let (new_insns,state) = run_all data pc_stack in
    Printf.printf "acc=%d\n" state.acc;
