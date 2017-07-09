open Cmdliner

let help_secs = [
 `S Manpage.s_common_options;
 `P "These options are common to all commands.";
 `S "MORE HELP";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";`Noblank;
 `P "Use `$(mname) help patterns' for help on patch matching."; `Noblank;
 `P "Use `$(mname) help environment' for help on environment variables.";
 `S Manpage.s_bugs; `P "Check bug reports at http://bugs.example.org.";]
;;

let encode_cmd =
  let encode in_file out_file =
    let out_file =
      if (String.length out_file) = 0 then
        in_file
      else
        out_file in
    Printf.printf "in : %s, out : %s\n" in_file out_file in
  let in_file =
    let doc = "File to encode" in
    Arg.(required & pos 0 (some file) None & info [] ~docv:"INFILE" ~doc) in
  let out_file =
    let doc = "Sbx container name" in
    Arg.(value & pos 1 string "" & info [] ~docv:"OUTFILE" ~doc) in
  let doc = "encode files" in
  Term.(const encode $ in_file $ out_file),
  Term.info "encode" ~doc
;;

let default_cmd =
  let doc = "a revision control system" in
  let sdocs = Manpage.s_common_options in
  let exits = Term.default_exits in
  let man = help_secs in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info "osbx" ~version:"v1.0.0" ~doc ~sdocs ~exits ~man
;;

let () = Term.exit @@ Term.eval_choice default_cmd [encode_cmd]
