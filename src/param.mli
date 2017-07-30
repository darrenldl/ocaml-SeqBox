module Common : sig
  val block_scan_alignment     : int
end

module Encode : sig
  val progress_report_interval : float
end

module Decode : sig
  val progress_report_interval : float
  val failure_list_max_length  : int64 ref
  val set_failure_list_max_length          : int64        -> unit
  val set_failure_list_max_length_possibly : int64 option -> unit
end

module Rescue : sig
  val progress_report_interval : float
  val log_write_interval       : float
end

module Show : sig
  val progress_report_interval : float
  val meta_list_max_length     : int64 ref
  val set_meta_list_max_length          : int64        -> unit
  val set_meta_list_max_length_possibly : int64 option -> unit
end
