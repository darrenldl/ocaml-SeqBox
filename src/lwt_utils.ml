module IO = struct
  let read_with_jitter ~(buf:bytes) (in_file:Lwt_io.input_channel) ~(len:int) : int Lwt.t =
    (* jitter = read where read count is smaller than requested but
     * there is still more data available in the channel
     *
     * this sometimes happen for some reason when reading large files
     * (1 GiB file was being used when this occured in testing)
    *)
    let jitter_threshold = 5 in
    let rec read_with_jitter_internal ~(read_so_far:int) ~(tries_left:int) : int Lwt.t =
      let%lwt read_count =
        Lwt_io.read_into in_file buf read_so_far (len - read_so_far) in
      let read_so_far : int = read_so_far + read_count in
      let tries_left  : int = tries_left - 1 in
      if      read_count < 0 then
        assert false
      else if read_count < len then
        if tries_left > 0 then
          read_with_jitter_internal ~read_so_far ~tries_left
        else
          Lwt.return read_so_far
      else
        Lwt.return read_so_far in
    read_with_jitter_internal ~read_so_far:0 ~tries_left:jitter_threshold
  ;;
end
