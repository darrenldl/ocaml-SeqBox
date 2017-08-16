open Cmdliner
open Rescue

let rescue (silent:Progress_report.silence_level option) (only_pick:Sbx_block.Block.block_type) (in_filename:string) (out_dirname:string) (log_filename:string option) : unit =
  Param.Common.set_silence_settings silent;
  match Process.rescue_from_file ~only_pick ~in_filename ~out_dirname ~log_filename with
  | Ok stats  -> Stats.print_stats stats
  | Error msg -> Printf.printf "%s\n" msg
;;

let only_pick =
  let doc = "Only pick $(docv) of blocks. $(docv) is one of : any, meta, data." in
  let open Sbx_block.Block in
  Arg.(value
       & opt
         (enum
            [("any", (`Any:block_type)); ("meta", (`Meta:block_type)); ("data", (`Data:block_type))])
         (`Any:block_type)
       & info
         ["only-pick"]
         ~docv:"TYPE"
         ~doc)
;;

let in_file =
  let doc = "File to rescue sbx data from" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"INFILE" ~doc)
;;

let out_dir =
  let doc = "Directory to store rescued data" in
  Arg.(required & pos 1 (some dir) None & info [] ~docv:"OUTDIR" ~doc)
;;

let log_file =
  let doc = "Logfile to keep track of progress to survive interruptions" in
  Arg.(value & pos 2 (some string) None & info [] ~docv:"LOGFILE" ~doc)
;;
