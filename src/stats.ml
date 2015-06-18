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

let update_rrds timestamp dss rrd =
  Rrd.ds_update_named rrd timestamp ~new_domid:false
    (List.map (fun ds -> ds.Ds.name, (ds.Ds.value, fun x -> x)) dss)

let rrd = create_fresh_rrd true (make_dss (Gc.stat ()))

let start ~sleep =
  let rec loop () =
    let timestamp = Clock.time () in
    update_rrds timestamp (make_dss (Gc.stat ())) rrd;
    sleep 5. >>= fun () ->
    loop () in
  Lwt.async loop

let page () =
  let sections = List.map (fun ds ->
    <:html<
    <h2>$str:ds.Rrd.ds_name$</h2>
    <div id=$str:ds.Rrd.ds_name$></div>
    >>
  ) (Array.to_list rrd.Rrd.rrd_dss) in

  <:html<
    <head>
      <meta charset="utf-8" />
      <link href="https://cdnjs.cloudflare.com/ajax/libs/c3/0.4.10/c3.css" rel="stylesheet" type="text/css"/>
      <script src="https://cdnjs.cloudflare.com/ajax/libs/d3/3.5.5/d3.min.js" charset="utf-8"></script>
      <script src="https://cdnjs.cloudflare.com/ajax/libs/c3/0.4.10/c3.min.js"></script>
      <script src="/js/stats/main.js"> </script>
    </head>
    <body>
      <h1>GC statistics</h1>
      <p>
        $List.concat sections$
      </p>
    </body>
  >>

let get_rrd_updates uri =
  let query = Uri.query uri in
  let get key =
    if List.mem_assoc key query
    then match List.assoc key query with
      | [] -> None
      | x :: _ -> Some x
    else None in
  let (>>=) m f = match m with None -> None | Some x -> f x in
  let default d = function None -> d | Some x -> x in
  let int64 x = try Some (Int64.of_string x) with _ -> None in
  let cf x = try Some (Rrd.cf_type_of_string x) with _ -> None in
  let start = default 0L (get "start" >>= int64) in
  let interval = default 0L (get "interval" >>= int64) in
  let cfopt = get "cf" >>= cf in
  Rrd_updates.export ~json:true [ "", rrd ] start interval cfopt
