external time : unit -> int64 = "caml_get_monotonic_time"

type t = { time : int64; mutable canceled : bool; thread : unit Lwt.u }

let sleepers = ref []

let ns d =
  let res, w = Lwt.task () in
  let time = Int64.add (time ()) d in
  let sleeper = { time; canceled = false; thread = w } in
  sleepers := sleeper :: !sleepers;
  Lwt.on_cancel res (fun _ -> sleeper.canceled <- true);
  res

let new_sleepers () =
  let sl = !sleepers in
  sleepers := [];
  sl
