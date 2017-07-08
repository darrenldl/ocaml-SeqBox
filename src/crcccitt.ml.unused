(* The following implementation is translated from libcrc
 *  URL : https://github.com/lammertb/libcrc (retrieved on 2017-06-27)
 * The files used as reference are:  
 *  crcccitt.c
 *  checksum.h
 *
 * Information of the original C implementation
 *  Library    : libcrc
 *  File       : src/crcccitt.c
 *  Author     : Lammert Bies
 *
 *  Library    : libcrc
 *  File       : include/checksum.h
 *  Author     : Lammert Bies
 *
 * Information of this OCaml translation
 *  File       : src/crcccitt.ml
 *  Translator : Darren Ldl
 *  Part of the ocaml-SeqBox project
 *
 * License (copied from the original C source code):
 * This file is licensed under the MIT License as stated below
 *
 * Copyright (c) 1999-2016 Lammert Bies
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *)
open Stdint

let crc_poly_ccitt       : int64 = Int64.of_int 0x1021;;

let crc_start_ccitt_1d0f : int64 = Int64.of_int 0x1d0f;;
let crc_start_ccitt_ffff : int64 = Int64.of_int 0xFFFF;;
let crc_start_xmodem     : int64 = Int64.of_int 0x0000;;

(* Do NOT rely on precedence of the following operators
 * as they are likely to be incorrect
 *
 * ALWAYS use a pair of paranthesis when multiple expressions are present
 *)
let (<) (a:int64) (b:int64) =
  let open Int64 in
  (compare a b) < 0
;;

let (>) (a:int64) (b:int64) =
  let open Int64 in
  (compare a b) > 0
;;

let (&) : int64 -> int64 -> int64 =
  Int64.logand;;

let (^) : int64 -> int64 -> int64 =
  Int64.logxor;;

let (<<) : int64 -> int -> int64 =
  Int64.shift_left;;

let (>>) : int64 -> int -> int64 =
  Int64.shift_right_logical;;

(* Translated from init_crcccitt_tab
 *
 * Behaviour difference:
 *  Generate the table then return it rather than
 *  writing to a global table then set a ready flag
 *)
let make_crcccitt_tab () : int64 array =
  let open Int64 in
  let (i      : int64 ref)   = ref (of_int 0) in
  let (crc    : int64 ref)   = ref (of_int 0) in
  let (c      : int64 ref)   = ref (of_int 0) in
  let (table  : int64 array) = Array.make 256 (of_int 0) in
  (* while loop is used here because value of i
   * is used in shifting,
   * thus requiring i to be int64,
   * which is impossible to do in for loop
   *)
  while (!i < of_int 256) do

    crc := of_int 0;
    c   := !i << 8;

    (* j in original version is not used *)
    for _ = 0 to 7 do

      if ((!crc ^ !c) & (of_int 0x8000)) > (of_int 0) then
        crc := begin ((!crc << 1) ^ crc_poly_ccitt) & (Int64.of_int 0xFFFF) end
      else
        crc := begin  (!crc << 1)                   & (Int64.of_int 0xFFFF) end;

      c := !c << 1
    done;

    table.(to_int !i) <- !crc;

    (* Increment for i *)
    i := !i + (of_int 1);
  done;
  table
;;

let crc_tabccitt = make_crcccitt_tab ();;

let update_crc_ccitt ~(crc:int64) ~(single_byte:uint8) : int64 =
  (crc << 8)
  ^
  crc_tabccitt.(
    Int64.to_int (
      (crc >> 8)
      ^
      (Int64.of_uint8 single_byte)
    )
  )
;;

let crc_ccitt_generic ~(input:bytes) ~(start_val:uint16) : uint16 =
  (* let crc : int64 ref = ref (Int64.of_int 0) in *)
  let crc : int64 ref = ref (Int64.of_int 0) in

  let val0x00FF       = Int64.of_int 0x00FF  in

  crc := (Uint16.to_int64 start_val);

  (*let (^)  = Int64.logxor in
  let (&)  = Int64.logand in
  let (<<) = Int64.shift_left in
  let (>>) = Int64.shift_right_logical in*)

  for i = 0 to (Bytes.length input) - 1 do
    crc := (!crc << 8)
           ^
           crc_tabccitt.(
             Int64.to_int (
               (
                 (!crc >> 8)
                 ^ 
                 (* let byte = (Uint8.of_bytes_big_endian input i) in
                    Int64.of_uint8 byte *)
                 Int64.of_int (Char.code (Bytes.get input i))
               )
               &
               val0x00FF
             )
           )
  done;
  (Uint16.of_int64 !crc)
;;

(*let crc_ccitt_ffff ~(input:bytes) : int64 =
  crc_ccitt_generic ~input ~start_val:crc_start_ccitt_ffff
;;

let crc_ccitt_1d0f ~(input:bytes) : int64 =
  crc_ccitt_generic ~input ~start_val:crc_start_ccitt_1d0f
;;

let crc_xmodem ~(input:bytes) : int64 =
  crc_ccitt_generic ~input ~start_val:crc_start_xmodem
;; *)

