open Stdint
open Fake_uint16

let crc_poly_ccitt = 0x1021L;;

let make_crcccitt_tab () : fuint16 array =
  let tab : fuint16 array = Array.make 256 0L in
  let i   : fuint16 ref   = ref 0L in
  let crc : fuint16 ref   = ref 0L in
  let c   : fuint16 ref   = ref 0L in

  while !i < 256L do
    crc := 0L;
    c   := !i << 8;

    for _ = 0 to 7 do
      if ((!crc ^ !c) & 0x8000L) > 0L then
        crc := (!crc << 1) ^ crc_poly_ccitt
      else
        crc :=  !crc << 1;

      c := !c << 1
    done;

    tab.(to_int !i) <- !crc;

    (* increment *)
    add1 i
  done;

  tab
;;

let crc_tabccitt = make_crcccitt_tab ();;

let crc_ccitt_generic ~(input:bytes) ~(start_val:fuint16) : fuint16 =
  let crc   : fuint16 ref = ref start_val in
  let index : int     ref = ref 0 in
  let len   : int         = Bytes.length input in

  for _ = 0 to len - 1 do
    crc := (!crc << 8)
           ^
           crc_tabccitt.(
             to_int (
               ((!crc >> 8) ^ (of_char (Bytes.get input !index)))
               &
               0x00FFL
             )
           );

    index := !index + 1
  done;

  !crc
;;

let crc_ccitt_generic_uint16 ~(input:bytes) ~(start_val:uint16) : uint16 =
  to_uint16 (
    crc_ccitt_generic ~input ~start_val:(of_uint16 start_val)
  )
;;
