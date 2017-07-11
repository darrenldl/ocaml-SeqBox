open Stream_file
open Sbx_block

module Stats = struct
end

module Processor = struct
  (* scan for valid block *)
  let scan_proc ~(skip_bytes:int64 option) (in_file:Core.In_channel.t) : Block.t option =
    let open Read_chunk in
    let len = Param.Rescue.scan_alignment in
    begin
      (* this is for resuming from log file *)
      match skip_bytes with
      | None   -> ()
      | Some n -> Core.In_channel.seek in_file n
    end;
    let bytes_to_block (raw_header:Header.raw_header) (chunk:bytes) : Block.t option =
      try
        Some (Block.of_bytes ~raw_header chunk)
      with
        | Header.Invalid_bytes
        | Metadata.Invalid_bytes
        | Block.Invalid_bytes
        | Block.Invalid_size     -> None in
    let rec scan_proc_internal () : Block.t option =
      match read in_file ~len with
      | None           -> None
      | Some { chunk } ->
        if Bytes.length chunk < 16 then
          None  (* no more bytes left in file *)
        else
          let test_header_bytes = Misc_utils.get_bytes chunk ~pos:0 ~len:16 in
          let test_header : Header.raw_header option =
            try
              Some (Header.of_bytes test_header_bytes)
            with
            | Header.Invalid_bytes -> None in
          match test_header with
          | None            -> scan_proc_internal ()
          | Some raw_header ->
            (* possibly grab more bytes depending on version *)
            let chunk =
              Processor_helpers.patch_block_bytes_if_needed in_file ~raw_header ~chunk in
            let test_block : Block.t option =
              bytes_to_block raw_header chunk in
            match test_block with
            | None       -> scan_proc_internal ()
            | Some block -> Some block  (* found a valid block *) in
    scan_proc_internal ()
  ;;

  (* append blocks to filename (use uid in hex string as filename)
   * return Error if failed to write for whatever reason
   *)
  let output_proc ~(block:Block.t) ~(out_dirname:string) : (unit, string) result =
    let out_filename =
      let uid_hex =
        Conv_utils.bytes_to_hex_string (Block.block_to_file_uid block) in
      let separator =
        if String.get out_dirname ((String.length out_dirname) - 1) = '/' then
          ""
        else
          "/" in
      String.concat separator [out_dirname; uid_hex] in
    let output_bytes =
      try
        Block.to_bytes block
      with
      (* there should not be any exceptions since the block was parsed from bytes
       * rather than crafted
      *)
      | Metadata.Too_much_data _ -> assert false in
    let output_proc_internal_processor (out_file:Core.Out_channel.t) : unit =
      let open Write_chunk in
      Core.Out_channel.seek out_file (Int64.sub (Core.Out_channel.length out_file) 1L); (* append to file *)
      write out_file ~chunk:output_bytes in
    match Stream.process_out ~out_filename ~processor:output_proc_internal_processor with
    | Ok _      -> Ok ()
    | Error msg -> Error msg
  ;;

  (* if there is any error with outputting, just print directly
   * this should be very rare however, if happening at all
   *)
  let rec scan_and_output ~(skip_bytes:int64 option) ~(out_dirname:string) (in_file:Core.In_channel.t) : unit =
    match scan_proc ~skip_bytes in_file with
    | None       -> ()  (* ran out of valid blocks in input file *)
    | Some block ->
      match output_proc ~block ~out_dirname with
      | Ok _      -> scan_and_output ~skip_bytes ~out_dirname in_file
      | Error msg -> Printf.printf "%s" msg; print_newline ()
  ;;

  let make_rescuer ~(skip_bytes:int64 option) ~(out_dirname:string) : unit Stream.in_processor =
    (fun in_file ->
       scan_and_output ~skip_bytes ~out_dirname in_file
    )
  ;;
end
