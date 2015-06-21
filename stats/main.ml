(* Copyright (c) 2015, Dave Scott <dave@recoil.org>

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*)

open Lwt

let do_get ~uri =
  let method_ = "GET" in
  let (res, w) = Lwt.task () in
  let req = XmlHttpRequest.create () in

  Firebug.console##log(Js.string (Uri.to_string uri));
  req##_open (Js.string method_, Js.string (Uri.to_string uri), Js._true);
  req##onreadystatechange <- Js.wrap_callback
    (fun _ ->
       (match req##readyState with
                   | XmlHttpRequest.DONE ->
                           Lwt.wakeup w (Js.to_string req##responseText)
                   | _ -> ()));

	req##send (Js.some (Js.string ""));
  Lwt.on_cancel res (fun () -> req##abort ()) ;
  res

let charts = Hashtbl.create 7

let colon = Re_str.regexp_string ":"

(* matches stats.ml *)
let timescales = [
  (120,     1); (* 120 values of interval 1 step (5 secs) = 10 mins  *)
  (120,    12); (* 120 values of interval 12 steps (1 min) = 2 hours *)
  (168,   720); (* 168 values of interval 720 steps (1 hr) = 1 week  *)
  (366, 17280); (* 366 values of interval 17280 steps (1 day) = 1 yr *)
]

let window =
  let n, interval = List.nth timescales 0 in
  n * interval * 5

let render_update update =
  let open Rrd_updates in
	let _, legends = Array.fold_left
	  (fun (idx, acc) elt ->
      match Re_str.split_delim colon elt with
      | [ "AVERAGE"; name ] ->
       (idx + 1, (idx, name) :: acc)
     | _ ->
       Firebug.console##log(Js.string (elt));
         (idx + 1, acc)
    ) (0, []) update.legend in

  let data = Array.to_list update.data in
  List.iter
    (fun (idx, legend) ->
	  	let points = List.map (fun x -> x.time, x.row_data.(idx)) data in
      let x_min = Int64.to_float update.Rrd_updates.end_time -. (float_of_int window) in
      Firebug.console##log(Js.string (Printf.sprintf "x_min = %f points = [| %s |]" x_min (String.concat "; " (List.map (fun (t, p) -> Printf.sprintf "%Ld %f" t p) points))));

      (* Filter out Nans *)
	  	let points = List.filter (fun (_, x) -> classify_float x <> FP_nan) points in
      let chart =
        if Hashtbl.mem charts legend
        then Hashtbl.find charts legend
        else begin
          let chart =
            C3.Line.make ~kind:`Timeseries ~x_format:"%H:%M:%S" ()
            |> C3.Line.render ~bindto:("#" ^ legend) in
          Hashtbl.add charts legend chart;
          chart
        end in
      if points <> []
      then C3.Line.flow ~segments:[ C3.Segment.make ~label:legend ~points:(List.map (fun (t, v) -> Int64.to_float t, v) points)
                               ~kind:`Area_step () ]
                   ~flow_to:(`ToX (`Time x_min))
                   chart;
    ) legends

let watch_rrds () =
  let uri start =
    let query = [ "start", [ string_of_int start ] ] in
    Uri.make ~scheme:"http" ~path:"/rrd_updates" ~query () in

  let rec loop start =
    do_get ~uri:(uri start)
    >>= fun txt ->
    let input = Xmlm.make_input (`String (0, txt)) in
    let update = Rrd_updates.of_xml input in
    Firebug.console##log(Js.string "got some updates");
    render_update update;
    Lwt_js.sleep 5.
    >>= fun () ->
    loop (Int64.to_int update.Rrd_updates.end_time) in
  loop (-window + 1)

let _ =
  Dom_html.window##onload <- Dom_html.handler
    (fun _ ->
      Lwt.async watch_rrds;
      Js._true
    )
