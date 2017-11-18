(* This file is translated from Galois.java from Backblaze Reed-Solomon implementation
 * as part of the ocaml-SeqBox project
 *
 * Following is the original notice
 *
 * 8-bit Galois Field
 *
 * Copyright 2015, Backblaze, Inc.  All rights reserved.
 *)
exception Zero_division

let field_size            : int = 256

let generating_polynomial : int = 29

let gen_log_table (polynomial : int) : int array =
  let result : int array = Array.make field_size 0 in

  for i = 0 to pred field_size do
    result.(i) <- -1
  done;

  let b : int ref = ref 1 in

  for log = 0 to pred (field_size - 1) do
    if result.(!b) <> -1 then (
      assert false
    );
    result.(!b) <- log;
    b := !b lsl 1;
    if field_size <= !b then (
      b := ((!b - field_size) lxor polynomial)
    )
  done;

  result
;;

let log_table = gen_log_table generating_polynomial

let gen_exp_table (log_table : int array) : int array =
  let result : int array = Array.make (field_size * 2 - 2) 0 in

  for i = 1 to pred field_size do
    let log : int = log_table.(i) in
    result.(log)                  <- i;
    result.(log + field_size - 1) <- i
  done;

  result
;;

let exp_table = gen_exp_table log_table

let add = (lxor)

let sub = (lxor)

let gen_mult_table () : int array array =
  let mult (a : int) (b : int) : int =
    if a = 0 || b = 0 then
      0
    else
      let logA = log_table.(a land 0xFF) in
      let logB = log_table.(b land 0xFF) in
      let res  = logA + logB in
      exp_table.(res) in

  let result : int array array = Array.make_matrix 256 256 0 in

  for a = 0 to pred field_size do
    for b = 0 to pred field_size do
      result.(a).(b) <- mult a b
    done;
  done;

  result
;;

let mult_table = gen_mult_table ()

let mult (a : int) (b : int) : int =
  if a = 0 || b = 0 then
    0
  else
    mult_table.(a).(b)
;;

let div (a : int) (b : int) : int =
  if      a = 0 then 0
  else if b = 0 then raise Zero_division
  else (
    let logA = log_table.(a land 0xFF) in
    let logB = log_table.(b land 0xFF) in
    let tmp  = logA - logB in
    let res  = if tmp < 0 then tmp + 255 else tmp in
    exp_table.(res)
  )
;;

let rec sub_255_until_lt_255 (x : int) : int =
  if 255 <= x then sub_255_until_lt_255 (x - 255)
  else             x
;;

let exp (a : int) (n : int) : int =
  if      n = 0 then 1
  else if a = 0 then 0
  else (
    let logA = log_table.(a land 0xFF) in
    let tmp  = logA * n in
    let res  = sub_255_until_lt_255 tmp in
    exp_table.(res)
  )
;;
