open Cmdliner

let encode in_file out_file =
  Printf.printf "in : %s, out : %s\n" in_file out_file
;;

let in_file =
  let doc = "File to encode" in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"INFILE" ~doc)
;;

let out_file =
  let doc = "Sbx container name" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"OUTFILE" ~doc)
;;

let encode_t = Term.(const encode $ in_file $ out_file);;

let encode_info =
  let doc = "encode files" in
  Term.info "encode" ~doc
;;

let main = Term.(const ());;

let info =
  let doc = "ocaml-SeqBox" in
  let man = [ `S Manpage.s_authors
            ; `P "Darren Ldl"
            ; `S Manpage.s_bugs
            ; `P "Open issues at ocaml-SeqBox github repo"
            ] in
  Term.info "osbx" ~doc ~exits:Term.default_exits ~man
;;

let () = Term.exit @@ Term.eval_choice (main, info) [(encode_t, encode_info)]
