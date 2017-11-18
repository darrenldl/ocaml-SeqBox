(* This file is translated from ReedSolomon.java from Backblaze Reed-Solomon implementation
 *
 * Reed-Solomon Coding over 8-bit values.
 *
 * Copyright 2015, Backblaze, Inc.
 *)
exception Too_many_shards
exception Shard_count_mismatch
exception Shard_length_mismatch
exception Invalid_range

type rs_ctx =
  { data_shard_count   : int
  ; parity_shard_count : int
  (*; total_shard_count  : int *)
  ; matrix             : int array array
  ; parity_rows        : int array array
  }

type shards_set =
  { data_shards   : string array
  ; parity_shards : string array
  }

let get_data_shard_count (ctx : rs_ctx) : int =
  ctx.data_shard_count
;;

let get_parity_shard_count (ctx : rs_ctx) : int =
  ctx.parity_shard_count
;;

let get_total_shard_count (ctx : rs_ctx) : int =
  ctx.data_shard_count + ctx.parity_shard_count
;;

(* Create a Vandermonde matrix, which is guaranteed to have the
 * property that any subset of rows that forms a square matrix
 * is invertible.
 *)
let build_vandermonde_matrix (rows : int) (cols : int) : int array array =
  let result : int array array = Array.make_matrix rows cols 0 in
  for r = 0 to pred rows do
    for c = 0 to pred cols do
      result.(r).(c) <- Galois.exp r c
    done;
  done;
  result
;;

(* Create the matrix to use for encoding, given the number of
 * data shards and the number of total shards.
 *
 * The top square of the matrix is guaranteed to be an identity
 * matrix, which means that the data shards are unchanged after
 * encoding.
 *)
let build_matrix ~(data_shard_count : int) ~(parity_shard_count : int) : int array array =
  let open Rs_matrix in
  let total_shard_count = data_shard_count + parity_shard_count in
  let vandermonde =
    build_vandermonde_matrix total_shard_count data_shard_count in
  let top =
    submatrix vandermonde 0 0 data_shard_count data_shard_count in
  mult vandermonde (invert top)
;;

let build_parity_rows (matrix : int array array) ~(parity_shard_count : int) : int array array =
  let open Rs_matrix in
  let parity_rows =
    Array.make_matrix parity_shard_count (col_count matrix) 0 in

  for i = 0 to pred parity_shard_count do
    for j = 0 to pred (col_count matrix) do
      parity_rows.(i).(j) <- matrix.(i).(j)
    done;
  done;

  parity_rows
;;

let make_ctx ~(data_shard_count : int) ~(parity_shard_count : int) : rs_ctx =
  if data_shard_count + parity_shard_count > Galois.field_size then
    raise Too_many_shards
  else
    let matrix = build_matrix ~data_shard_count ~parity_shard_count in
    let parity_rows = build_parity_rows matrix ~parity_shard_count in

    { data_shard_count
    ; parity_shard_count
    ; matrix
    ; parity_rows
    }
;;

let check_buffer_and_sizes ~(ctx : rs_ctx) ~(shards : shards_set) ~(offset : int) ~(byte_count) : unit =
  if Array.length shards.data_shards <> get_total_shard_count ctx then
    raise Shard_count_mismatch;

  let first_shard_len = Array.length shards.data_shards.(0) in
  for i = 1 to Array.length shards.data_shards do
    if Array.length shards.data_shards.(i) <> first_shard_len then
      raise Shard_length_mismatch
  done;
  for i = 1 to Array.length shards.parity_shards do
    if Array.length shards.parity_shards.(i) <> first_shard_len then
      raise Shard_length_mismatch
  done;

  if offset < 0 then
    raise Invalid_range;
  if byte_count < 0 then
    raise Invalid_range;
  if first_shard_len < offset + byte_count then
    raise Invalid_range
;;

(* Encodes parity for a set of data shards. *)
let encode_parity ~(ctx : rs_ctx) ~(shards : shards_set) ~(offset : int) ~(byte_count : int) : unit =
  check_buffer_and_sizes ~ctx ~shards ~offset ~byte_count;

let is_parity_correct ~(shards : shards_set) 
