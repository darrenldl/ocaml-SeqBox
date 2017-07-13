module Encode : sig
  val progress_report_interval : float
end

module Decode : sig
  val ref_block_scan_alignment : int
  val progress_report_interval : float
end

module Rescue : sig
  val scan_alignment           : int
  val progress_report_interval : float
  val log_write_interval       : float
end
