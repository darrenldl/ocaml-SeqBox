module Common = struct
  let block_scan_alignment     = 128  (* largest common divisor of version 1, 2, 3 block sizes *)
end

module Encode = struct
  let progress_report_interval = 0.1  (* update every 100ms *)
end

module Decode = struct
  let ref_block_scan_alignment = Common.block_scan_alignment
  let progress_report_interval = 0.1  (* update every 100ms *)
end

module Rescue = struct
  let scan_alignment           = Common.block_scan_alignment
  let progress_report_interval = 0.1  (* update every 100ms *)
  let log_write_interval       = 0.1  (* write  every 100ms *)
end
