(*
 * Copyright (c) 2011-2015 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2013      Citrix Systems Inc
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** {1 Time device}

    This module defines a target-agnostic MirageOS sleep operation.  *)

val ns: int64 -> unit Lwt.t
(** [sleep_ns n] blocks the current task for [n] nanoseconds, treating [n] as
    unsigned.  *)

(** {1 Values used by the schedulers} *)

type t = { time : int64; mutable canceled : bool; thread : unit Lwt.u }
(** The type for a sleeping task. *)

val new_sleepers : unit -> t list
(** [new_sleepers ()] is used by the scheduler to find at their convenience
    the tasks that need to be enqueued into their task set. This also empties
    the list of sleepers. *)
