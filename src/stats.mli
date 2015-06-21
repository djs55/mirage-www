val start: sleep:(float -> unit Lwt.t) -> unit
val page: unit -> Cow.Html.t

val get_rrd_updates: Uri.t -> string

val get_rrd_timescales: Uri.t -> string
