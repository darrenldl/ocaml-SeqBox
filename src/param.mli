module Encode : sig
  val progress_report_interval : int
end

module Decode : sig
  val ref_block_scan_alignment : int
  val progress_report_interval : int
end

module Rescue : sig
  val scan_alignment           : int
end
