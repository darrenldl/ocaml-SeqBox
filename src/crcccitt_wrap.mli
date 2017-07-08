open Stdint

val crc_ccitt_generic  : input:string -> start_val:uint16 -> uint16

val crc_ccitt_sbx_ver1 : input:string -> uint16
