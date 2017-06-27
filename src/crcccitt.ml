(* The following code is translated from libcrc
 *  URL : https://github.com/lammertb/libcrc (retrieved on 2017-06-27)
 * The files used as referenced are:  
 *  crcccitt.c
 *  checksum.h
 *)
open Stdint

let crc_poly_ccitt : uint16 = Uint16.of_int 0x1021;;
let crc_poly_ffff  : uint16 = Uint16.of_int 0xFFFF;;

(* Do NOT rely on precedence of the following operators
 * as they are likely to be incorrect
 *
 * ALWAYS use a pair of paranthesis when multiple expressions are present
 *)
let (<) (a:uint16) (b:uint16) =
  let open Uint16 in
  let res = compare a b in
  if   res < 0 then
    true
  else
    false;;

let (>) (a:uint16) (b:uint16) =
  let open Uint16 in
  let res = compare a b in
  if   res > 0 then
    true
  else
    false;;

let (&) : uint16 -> uint16 -> uint16 =
  Uint16.logand;;

let (^) : uint16 -> uint16 -> uint16 =
  Uint16.logxor;;

let (<<) : uint16 -> int -> uint16 =
  Uint16.shift_left;;

let (>>) : uint16 -> int -> uint16 =
  Uint16.shift_right;;

let make_crcccitt_tab () : uint16 array =
  let open Uint16 in
  let (i      : uint16 ref)   = ref (of_int 0) in
  let (crc    : uint16 ref)   = ref (of_int 0) in
  let (c      : uint16 ref)   = ref (of_int 0) in
  let (table  : uint16 array) = Array.make 256 (of_int 0) in
  (* while loop is used here because value of i
   * is used in shifting,
   * thus requiring i to be uint16,
   * which is impossible to do in for loop
   *)
  while (!i < of_int 256) do

    crc := of_int 0;
    c   := !i << 8;

    (* j in original version is not used *)
    for _ = 0 to 7 do

      if ((!crc ^ !c) & (of_int 0x8000)) > (of_int 0) then
        crc := begin (!crc << 1) ^ crc_poly_ccitt end
      else
        crc := begin  !crc << 1                   end;

      c := !c << 1
    done;

    table.(to_int !i) <- !crc;

    (* Increment for i *)
    i := !i + (of_int 1);
  done;
  table
;;

let crc_tabccitt = make_crcccitt_tab ();;

let crc_ccitt_generic ~(input:bytes) ~(start_val:uint16) : uint16 =
  let open Uint16 in
  let (crc   : uint16 ref) = ref (of_int 0) in

  crc   := start_val;

  for i = 0 to Bytes.length input do
    crc := (!crc << 8)
           ^
           crc_tabccitt.(
             to_int (
               (
                 (!crc >> 8)
                 ^ 
                 (of_bytes_big_endian input i)
               )
               &
               (of_int 0x00FF)
             )
           )
  done;
  !crc
;;

let crc_ccitt_ffff ~(input:bytes) : uint16 =
  crc_ccitt_generic ~input ~start_val:crc_poly_ffff
;;


