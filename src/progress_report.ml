let seconds_to_hms (total_secs:int) : int * int * int =
  let hour               : int   = total_secs  / (60 * 60) in
  let minute             : int   = (total_secs - hour * 60 * 60) / 60 in
  let second             : int   = (total_secs - hour * 60 * 60 - minute * 60) in
  (hour, minute, second)
;;

let print_generic ~(header:string) ~(print_every_n:int) =
  let last_report_time    : float ref = ref 0. in
  let last_reported_units : int64 ref = ref 0L in
  let print_count         : int   ref = ref 0  in
  (fun ~(start_time:float) ~(units_so_far:int64) ~(total_units:int64) : unit ->
     let percent : int =
       Int64.to_int (Int64.div
                       (Int64.mul
                          100L
                          units_so_far)
                       total_units) in
     if percent = 100 (* always print if reached 100% *) || !print_count = 0 then 
       begin
         let cur_time                           = Sys.time () in
         let time_since_last_report : float     = cur_time -. !last_report_time in
         let cur_rate               : float     = ((Int64.to_float units_so_far) /. time_since_last_report) in
         let time_elapsed           : float     = cur_time -. start_time in
         let avg_rate               : float     = (Int64.to_float units_so_far) /. time_elapsed in
         let units_remaining        : int64     = Int64.sub total_units units_so_far in
         let etc_total_secs         : int       = int_of_float (ceil ((Int64.to_float units_remaining) /. avg_rate)) in
         let (etc_hour, etc_minute, etc_second) = seconds_to_hms etc_total_secs in
         last_report_time    := cur_time;
         last_reported_units := units_so_far;
         Printf.printf "\r%s : %Ld / %Ld - %d%%  cur : %.0f/s  etc : %d:%d:%d          "
           header
           units_so_far
           total_units
           percent
           cur_rate
           etc_hour
           etc_minute
           etc_second
       end;
     if percent = 100 then
       print_newline ();
     (* increase and mod print counter *)
     print_count := (!print_count + 1) mod print_every_n
  )
;;
