open Sbx_block

let test () : unit =
  let common     = Header.make_common_fields `V1 in
  let data_block = Block.make_data_block ~seq_num:(Uint32.of_int 1) common ~data:(Bytes.make 496 'a') in
  let data_block_bytes = Block.to_bytes data_block in

  for _ = 0 to 2115 do
    Blocks.of_bytes data_block_bytes |> ignore
  done
;;

test ()
