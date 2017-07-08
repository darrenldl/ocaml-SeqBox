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
 *  File       : src/crcccitt.mli
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

val crc_ccitt_generic : input:bytes -> start_val:uint16  -> uint16
