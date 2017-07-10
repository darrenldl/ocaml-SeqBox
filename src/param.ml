module Decode = struct
  let ref_block_scan_alignment = 128 (* largest common divisor of version 1, 2, 3 block sizes *)
end

module Rescue = struct
  let scan_alignment = 128 (* largest common divisor of version 1, 2, 3 block sizes *)
end
