open Cmdliner

let chorus count msg =
  for i = 1 to count do print_endline msg done
;;

let count =
  let doc = "Repeat the message $(docv) times." in
  Arg.(value & opt int 10 & info ["c"; "count"] ~docv:"COUNT" ~doc)
;;

let msg =
  let doc = "Overries the default message to print." in
  let env = Arg.env_var "CHORUS_MSG" ~doc in
  let doc = "The message to print." in
  Arg.(value & pos 0 string "Revolt!" & info [] ~env ~docv:"MSG" ~doc)
;;

let chorus_t = Term.(const chorus $ count $ msg);;

let info =
  let doc = "print a customizable message repeatedly" in
  let man = [ `S Manpage.s_bugs
            ; `P "Email bug reports to <whatever at wherever>."
            ] in
  Term.info "chorus" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits ~man
;;

let print () = print_endline "Hello";;

let tmsg = Arg.value ();;

let test_t = Term.(const print $ tmsg);;

let test_info =
  let doc = "whatever" in
  let man = [ `S Manpage.s_authors
            ; `P "Darren"
            ] in
  Term.info "tt" ~version:"%%VERSION%%" ~doc ~man
;;

let () = Term.exit @@ Term.eval_choice (chorus_t, info) [(test_t, test_info)]
