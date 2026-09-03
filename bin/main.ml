let usage_msg = "interpreter <file> -o [output]"
let input_file = ref ""
let output_file = ref ""

let anon_fun filename = 
    if !input_file <> "" then
        raise (Arg.Bad "only one input file may be provided")
    else
        input_file := filename


let speclist =  [("-o", Arg.Set_string output_file, "set output file name")]


let () = 
    Arg.parse speclist anon_fun usage_msg;
    let interpreter = new Interpreter_project.Lox.lox in
    
    if !input_file = "" then begin
        interpreter#run_prompt ()
    end;

    let _global_env = Interpreter_project.Envr.make in
    let token_list = interpreter#run_file(!input_file) in
    let parser = Interpreter_project.Parser.make token_list in
    let stmts = Interpreter_project.Parser.delcaration parser in
    let _interpret = Interpreter_project.Interpreter.execute stmts _global_env in
    ()


