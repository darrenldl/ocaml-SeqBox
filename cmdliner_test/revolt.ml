open Cmdliner

let revolt () = print_endline "Revolt!";;

let revolt_t = Term.(const revolt $ const ())

let () = Term.exit @@ Term.eval (revolt_t, Term.info "revolt")
