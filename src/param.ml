open Progress_report

module Common = struct
  let block_scan_alignment     = 128  (* largest common divisor of version 1, 2, 3 block sizes *)
end

module Encode = struct
  let progress_report_interval = 0.3  (* update every  300ms *)

  module Encode_progress = struct
    let display_while_active    = [`Progress_bar; `Percentage; `Current_rate; `Time_used; `Time_left]
    let display_on_finish       = []
    let display_on_finish_early = []
  end
end

module Decode = struct
  let progress_report_interval = 0.3  (* update every  300ms *)

  module Scan_progress = struct
    let display_while_active    = [`Progress_bar; `Percentage; `Current_rate; `Time_used; `Time_left]
    let display_on_finish       = []
    let display_on_finish_early = []
  end

  module Hash_progress = struct
    let display_while_active    = [`Progress_bar; `Percentage; `Current_rate; `Time_used; `Time_left]
    let display_on_finish       = []
    let display_on_finish_early = []
  end

  module Decode_progress = struct
    let display_while_active    = [`Progress_bar; `Percentage; `Current_rate; `Time_used; `Time_left]
    let display_on_finish       = []
    let display_on_finish_early = []
  end

  let failure_list_max_length  = ref 100L

  let set_failure_list_max_length (n:int64) : unit =
    failure_list_max_length := n
  ;;

  let set_failure_list_max_length_possibly (n:int64 option) : unit =
    match n with
    | Some n -> failure_list_max_length := n
    | None   -> ()
  ;;
end

module Rescue = struct
  let progress_report_interval = 0.3  (* update every  300ms *)

  module Rescue_progress = struct
    let display_while_active    = [`Progress_bar; `Percentage; `Current_rate; `Time_used; `Time_left]
    let display_on_finish       = []
    let display_on_finish_early = []
  end

  let log_write_interval       = 1.0  (* write  every 1000ms *)
end

module Show = struct
  let progress_report_interval = 0.3  (* update every  300ms *)

  module Show_progress = struct
    let display_while_active    = [`Progress_bar; `Percentage; `Current_rate; `Time_used; `Time_left]
    let display_on_finish       = []
    let display_on_finish_early = []
  end

  let meta_list_max_length     = ref 0L

  let set_meta_list_max_length (n:int64) : unit =
    meta_list_max_length := n
  ;;

  let set_meta_list_max_length_possibly (n:int64 option) : unit =
    match n with
    | Some n -> meta_list_max_length := n
    | None   -> ()
  ;;
end
