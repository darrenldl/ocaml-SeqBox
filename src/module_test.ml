module T = struct
  let a = 1

  type t = A

  module K = struct
    let x : t = A
  end

  module K2 = struct
    let x = a
  end
end

module T2 = struct
  module K = struct
    let k = T.K.x
  end
end

let test () =
  Printf.printf "%d\n" T.K.x
;;

test ()
