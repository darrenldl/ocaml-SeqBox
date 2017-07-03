open Sbx_version
open Sbx_block
open Stream_file

module Processor : sig
  type stats = { blocks_written : int
               }

  val make_in_out_encoder : common:Header.common_fields -> metadata:(Metadata.t list) option -> stats Stream.in_out_processor
end
