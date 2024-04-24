external time : unit -> int64 = "caml_get_monotonic_time"

let sleep_ns d =
  let res, w = Lwt.task () in
  let time = Int64.add (time ()) d in
  let sleeper = Mirage_runtime.{ time; canceled = false; thread = w } in
  Mirage_runtime.add_new_sleeper sleeper;
  Lwt.on_cancel res (fun _ -> sleeper.canceled <- true);
  res
