open Stdint

(* Only version 1 is supported as of time of writing *)
type version = [ `One ]

type header = 
  { signature  : bytes
  ; version    : version
  ; crc16ccitt : bytes
  ; file_uid   : bytes
  ; seq_num    : uint32
  }

type metadata =
  { id   : bytes
  ; len  : uint8
  ; data : bytes
  }

type meta_block =
  { header : header
  ; data   : metadata list
  }

type data_block =
  { header : header
  ; data   : bytes
  }

type last_data_block =
  { header  : header
  ; data    : bytes
  ; padding : bytes
  }

type block_type = [ `Meta | `Data | `Last_data ]

type t = meta_block | data_block | last_data_block

type res = (t, string) result
