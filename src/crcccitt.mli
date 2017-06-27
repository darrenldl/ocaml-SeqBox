(* The implementation code is translated from libcrc
 *  URL : https://github.com/lammertb/libcrc (retrieved on 2017-06-27)
 * The files used as referenced are:  
 *  crcccitt.c
 *  checksum.h
 *)
open Stdint

val update_crc_ccitt  : crc:uint16  -> single_byte:uint8 -> uint16

val crc_ccitt_generic : input:bytes -> start_val:uint16  -> uint16

val crc_ccitt_ffff    : input:bytes -> uint16

val crc_ccitt_1d0f    : input:bytes -> uint16

val crc_xmodem        : input:bytes -> uint16
