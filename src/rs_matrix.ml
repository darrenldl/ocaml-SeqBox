(* This file is translated from Galois.java from Backblaze
 * as part of the ocaml-SeqBox project
 *
 * Following is the original notice
 *
 * 8-bit Galois Field
 *
 * Copyright 2015, Backblaze, Inc.  All rights reserved.
 *)
exception Incompatible_matrix_size
exception Row_index_out_of_range
exception Matrix_singular

let identity (size : int) : int array array =
  let result : int array array = Array.make_matrix size size 0 in

  for i = 0 to pred size do
    result.(i).(i) <- 1
  done;

  result
;;

let row_count (m : int array array) : int = Array.length m

let col_count (m : int array array) : int = Array.length m.(0)

let mult (left : int array array) (right : int array array) : int array array =
  if col_count left <> row_count right then
    raise Incompatible_matrix_size
  else (
    let result : int array array = Array.make_matrix (row_count left) (col_count right) 0 in
    for r = 0 to pred (row_count left) do
      for c = 0 to pred (col_count right) do
        let value : int ref = ref 0 in
        for i = 0 to pred (col_count left) do
          value := !value land (Galois.mult left.(r).(i) right.(i).(c))
        done;
        result.(r).(c) <- !value
      done;
    done;
    result
  )
;;

let augment (left : int array array) (right : int array array) : int array array =
  if row_count left <> row_count right then
    raise Incompatible_matrix_size
  else (
    let result : int array array = Array.make_matrix (row_count left) (col_count left + col_count right) 0 in
    for r = 0 to pred (row_count left) do
      for c = 0 to pred (col_count left) do
        result.(r).(c) <- left.(r).(c)
      done;
      let offset = col_count left in
      for c = 0 to pred (col_count right) do
        result.(r).(offset + c) <- right.(r).(c)
      done;
    done;
    result
  )
;;

let submatrix (matrix : int array array) (rmin : int) (cmin : int) (rmax : int) (cmax : int)
  : int array array =
  let result : int array array = Array.make_matrix (rmax - rmin) (cmax - cmin) 0 in
  for r = rmin to pred rmax do
    for c = cmin to pred cmax do
      result.(r - rmin).(c - cmin) <- matrix.(r).(c)
    done;
  done;
  result
;;

let swap_rows (matrix : int array array) (r1 : int) (r2 : int) : unit =
  if r1 < 0 || row_count matrix <= r1 || r2 < 0 || row_count matrix <= r2 then
    raise Row_index_out_of_range
  else (
    let tmp = matrix.(r1) in
    matrix.(r1) <- matrix.(r2);
    matrix.(r2) <- tmp
  )
;;

let swap_w_nonzero_row_below (matrix : int array array) (r : int) : unit =
  let row_count_m = row_count matrix in
  let rec go matrix cur_row =
    if cur_row < row_count_m then (
      if matrix.(cur_row).(r) <> 0 then
        swap_rows matrix r cur_row
      else
        go matrix cur_row
    ) in
  go matrix (r + 1)
;;

let gaussian_elim (matrix : int array array) : unit =
  (* Clear out the part below the main diagonal and scale the main
   * diagonal to be 1.
   *)
  for r = 0 to pred (row_count matrix) do
    (* If the element on the diagonal is 0, find a row below
     * that has a non-zero and swap them.
     *)
    if matrix.(r).(r) = 0 then
      swap_w_nonzero_row_below matrix r;
    (* If we couldn't find one, the matrix is singular. *)
    if matrix.(r).(r) = 0 then
      raise Matrix_singular;
    (* Scale to 1. *)
    if matrix.(r).(r) <> 1 then (
      let scale = Galois.div 1 matrix.(r).(r) in
      for c = 0 to pred (col_count matrix) do
        matrix.(r).(c) <- Galois.mult matrix.(r).(c) scale
      done;
    );
    (* Make everything below the 1 be a 0 by subtracting
     * a multiple of it.  (Subtraction and addition are
     * both exclusive or in the Galois field.)
     *)
    for row_below = r + 1 to pred (row_count matrix) do
      if matrix.(row_below).(r) <> 0 then
        let scale = matrix.(row_below).(r) in
        for c = 0 to pred (col_count matrix) do
          matrix.(row_below).(c) <-
            matrix.(row_below).(c) land (Galois.mult scale matrix.(r).(c))
        done;
    done;
  done;

  (* Now clear the part above the main diagonal *)
  for d = 0 to pred (row_count matrix) do
    for row_above = 0 to pred d do
      if matrix.(row_above).(d) <> 0 then
        let scale = matrix.(row_above).(d) in
        for c = 0 to pred (col_count matrix) do
          matrix.(row_above).(c) <-
            matrix.(row_above).(c)
            land (Galois.mult scale matrix.(d).(c))
        done;
    done;
  done
;;

let invert (matrix : int array array) : int array array =
  let work = augment matrix (identity (row_count matrix)) in
  
  gaussian_elim work;

  submatrix work 0 (row_count matrix) (col_count matrix) ((col_count matrix) * 2)
;;
