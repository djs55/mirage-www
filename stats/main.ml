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

let render_update update =
  (* XXX: pick a memory free RRD for now *)
  let open Rrd_updates in
	let _, legends = Array.fold_left
	  (fun (idx, acc) elt ->
      match Xen_api_metrics.Legend.of_string elt with
       | `Ok (name, _, _, _) ->
         (idx + 1, (idx, name) :: acc)
       | _ ->
         (idx + 1, acc)
    ) (0, []) update.legend in
  let data = Array.to_list update.data in
  List.iter
    (fun (idx, legend) ->
  	  (* Firebug.console##log(Js.string(Printf.sprintf "legends = %d; nrows = %d; nrow_datas = [ %s ]" (Array.length update.legend) (Array.length update.data) (String.concat "; " (List.map (fun x -> string_of_int (Array.length x.row_data)) (Array.to_list update.data)))));
    *)
	  	let points = List.map (fun x -> x.time, x.row_data.(idx)) data in
      (* Filter out Nans *)
	  	let points = List.filter (fun (_, x) -> classify_float x <> FP_nan) points in
	  	Firebug.console##log(Js.string (Printf.sprintf "points = [| %s |]" (String.concat "; " (List.map (fun (t, p) -> Printf.sprintf "%Ld %f" t p) points))));
      (* XXX: I have no idea what these values mean! *)
	  	(*let point = sin(time /. 60.) *. point *. 0.2 +. point *. 0.8 in*)
      let chart =
        if Hashtbl.mem charts legend
        then Hashtbl.find charts legend
        else begin
          let chart =
            C3.Line.make ~kind:`Timeseries ~x_format:"%m/%d" ()
            |> C3.Line.render ~bindto:("#" ^ legend) in
          Hashtbl.add charts legend chart;
          chart
        end in
      C3.Line.flow ~segments:[ C3.Segment.make ~label:legend ~points:(List.map (fun (t, v) -> Int64.to_float t, v) points)
                               ~kind:`Area_step () ]
                   ~flow_to:(`Delete 1)
                   chart;
    ) legends

let watch_rrds () =
  let uri start =
    let query = [ "start", [ string_of_int start ] ] in
    Uri.make ~scheme:"http" ~path:"/rrd_updates" ~query () in

  let rec loop start =
    do_get ~uri:(uri start)
    >>= fun txt ->
    let update = Xen_api_metrics.Updates.parse txt in
    Firebug.console##log(Js.string "got some updates");
    render_update update;
    Lwt_js.sleep 5.
    >>= fun () ->
    loop (Int64.to_int update.Rrd_updates.end_time) in
  (* XXX: query server's current clock *)
  do_get ~uri:(uri 1500000000) >>= fun txt ->
  let update = Xen_api_metrics.Updates.parse txt in
  loop ((Int64.to_int update.Rrd_updates.end_time) - 60*9)

let _ =
  Dom_html.window##onload <- Dom_html.handler
    (fun _ ->
      Lwt.async watch_rrds;
      Js._true
    )
