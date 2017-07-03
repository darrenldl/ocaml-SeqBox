open Angstrom
open Stdint

let int_to_ver (x:int) : string =
  if x = 1 then
    "version 1"
  else
    assert false
;;

let ver_p =
  lift (int_to_ver) (char '\x01' >>| int_of_char)
;;

let header_parser =
  string "SBx" *> ver_p
;;

match parse_only header_parser (`String "SBx\x02") with
| Ok s -> Printf.printf "Okay : %s\n" s
| Error msg -> Printf.printf "Error : %s\n" msg
