class lox =
  object (self)
    method run_file input_file =
      let file_string =
        In_channel.with_open_text
          input_file
          In_channel.input_all
      in
      let scanner = new Scanner.scanner file_string self#error in
      scanner#scan_tokens ()

    method report line where message =
      Printf.printf
        "[line %d] Error %s: %s\n"
        line
        where
        message

    method error (line, message) =
      self#report line "" message

    method run source =
      Printf.printf "%s\n" source

    method run_prompt () =
      let rec loop () =
        print_string "> ";
        flush stdout;

        match read_line () with
        | line ->
            self#run line;
            loop ()
        | exception End_of_file ->
            ()
      in
      loop ()
  end