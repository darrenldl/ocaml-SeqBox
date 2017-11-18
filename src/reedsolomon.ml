(* This file is translated from ReedSolomon.java from Backblaze Reed-Solomon implementation
 *
 * Reed-Solomon Coding over 8-bit values.
 *
 * Copyright 2015, Backblaze, Inc.
 *)
type rs_param =
  { data_shard_count   : int
  ; parity_shard_count : int
  ; total_shard_count  : int
  ; matrix             : int array array
  }

