let (>>=) = Lwt.bind

open Rrd

let timescales = [
  (120,     1); (* 120 values of interval 1 step (5 secs) = 10 mins  *)
  (120,    12); (* 120 values of interval 12 steps (1 min) = 2 hours *)
  (168,   720); (* 168 values of interval 720 steps (1 hr) = 1 week  *)
  (366, 17280); (* 366 values of interval 17280 steps (1 day) = 1 yr *)
]

let create_rras use_min_max =
  (* Create archives of type min, max and average and last *)
  Array.of_list (List.flatten
    (List.map (fun (n,ns) ->
      if ns > 1 && use_min_max then [
        Rrd.rra_create Rrd.CF_Average n ns 1.0;
        Rrd.rra_create Rrd.CF_Min n ns 1.0;
        Rrd.rra_create Rrd.CF_Max n ns 1.0;
      ] else [Rrd.rra_create Rrd.CF_Average n ns 0.5]
    ) timescales)
  )

let step = 5

module Ds = struct
  type t = {
    name: string;
    description: string;
    value: Rrd.ds_value_type;
    ty: Rrd.ds_type;
    max: float;
    min: float;
    units: string;
  }
  let make ~name ~description ~value ~ty ~units
    ?(min = neg_infinity) ?(max = infinity) () = {
    name; description; value; ty; min; max; units
  }
end

let make_dss stats =
  let total_kib = Int64.of_int (stats.Gc.heap_words / 256) in
  let actualfree_kib = Int64.of_int (stats.Gc.free_words / 256) in
  let actuallive_kib = Int64.of_int (stats.Gc.live_words / 256) in
  let grad_kib = (stats.Gc.minor_words +. stats.Gc.major_words -. stats.Gc.promoted_words) /. 256. in [
  Ds.make ~name:"memory_usage" ~units:"KiB"
    ~description:"Total memory allocated used"
    ~value:(Rrd.VT_Int64 total_kib) ~ty:Rrd.Gauge ~min:0.0 ();
  Ds.make ~name:"free_memory" ~units:"KiB"
    ~description:"Free memory available"
    ~value:(Rrd.VT_Int64 actualfree_kib) ~ty:Rrd.Gauge ~min:0.0 ();
  Ds.make ~name:"live_memory" ~units:"KiB"
    ~description:"Live memory used"
    ~value:(Rrd.VT_Int64 actuallive_kib) ~ty:Rrd.Gauge ~min:0.0 ();
  Ds.make ~name:"allocation" ~units:"KiB"
    ~description:"Memory allocation done"
    ~value:(Rrd.VT_Float grad_kib) ~ty:Rrd.Derive ~min:0.0 ();
  ]

(** Create a rrd *)
let create_fresh_rrd use_min_max dss =
  let rras = create_rras use_min_max in
  let dss = Array.of_list (List.map (fun ds ->
      Rrd.ds_create ds.Ds.name ds.Ds.ty ~mrhb:300.0 ~max:ds.Ds.max
      ~min:ds.Ds.min Rrd.VT_Unknown 
    ) dss) in
  let rrd = Rrd.rrd_create dss rras (Int64.of_int step) (Unix.gettimeofday()) in
  rrd

let delay = 60. *. 2.
let history = 100

let html_of_stat t =
  let open Gc in
  let k f =
    let str = Printf.sprintf "%dk" (f / 1_000) in
    Cow.Html.of_string str
  in
  let m f =
    let str = Printf.sprintf "%.0fm" (f /. 1_000_000.) in
    Cow.Html.of_string str
  in
  <:html<
      <tr>
      <td>$m (Gc.allocated_bytes ())$</td>
      <td>$k t.heap_words$</td>
      <td>$k t.live_words$</td>
      </tr>
    >>

let html_of_stats ts =
  <:html<
    <table>
    <tr>
    <th>Allocated Bytes</th>
    <th>Heap Words</th>
    <th>Live Words</th>
    </tr>
    $list:List.map html_of_stat ts$
    </table>
   >>

let stats = Queue.create ()

let start ~sleep =
  let gather () =
    let stat = Gc.stat () in
    if Queue.length stats >= history then ignore (Queue.pop stats);
    Queue.push stat stats
  in
  let rec loop () =
    gather ();
    sleep delay >>= fun () ->
    loop ()
  in
  Lwt.async loop

let page () =
  let stats = Queue.fold (fun acc s -> s :: acc) [] stats in
  html_of_stats stats
