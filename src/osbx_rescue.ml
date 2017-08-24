open Cmdliner
open Rescue

let rescue (silent:Progress_report.silence_level) (only_pick:Sbx_block.Block.block_type) (from_byte:int64 option) (to_byte:int64 option) (in_filename:string) (out_dirname:string) (log_filename:string option) : unit =
  Param.Common.set_silence_settings silent;
  match Process.rescue_from_file ~only_pick ~from_byte ~to_byte ~in_filename ~out_dirname ~log_filename with
  | Ok stats  -> Stats.print_stats stats
  | Error msg -> Printf.printf "%s\n" msg
;;

let only_pick =
  let doc = "Only pick $(docv) of blocks. $(docv) is one of : any, meta, data." in
  let open Sbx_block.Block in
  Arg.(value
       & opt
         (enum [("any",  (`Any:block_type));
                ("meta", (`Meta:block_type));
                ("data", (`Data:block_type))])
         (`Any:block_type)
       & info
         ["only-pick"]
         ~docv:"TYPE"
         ~doc)
;;

let from_byte =
  let doc = Printf.sprintf "Start from byte $(docv), the position is automatically rounded down to closest multiple of %d bytes.
  If not specified, defaults to start of file.
  Negative values are treated as 0."
      Param.Common.block_scan_alignment in
  Arg.(value & opt (some int64) None & info ["from"] ~docv:"FROM-BYTE" ~doc)
;;

let to_byte =
  let doc = "Last position to try to decode a block.
  If not specified, defaults to end of file.
  Negative values are treated as 0.
  If $(docv) is smaller than FROM-BYTE, then it will be treated as FROM-BYTE." in
  Arg.(value & opt (some int64) None & info ["to"] ~docv:"TO-BYTE" ~doc)
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
