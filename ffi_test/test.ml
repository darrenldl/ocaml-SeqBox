open Stdint
open Ctypes
open Foreign

let hello =
  foreign "hello" (void @-> returning void)
;;

let test () =
  hello () 
;;

test ()
