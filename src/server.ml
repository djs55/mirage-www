open Printf
open Lwt

let port = Config.port

let ip =
  let open Net.Nettypes in
  ( ipv4_addr_of_tuple (10l,0l,0l,2l),
    ipv4_addr_of_tuple (255l,255l,255l,0l),
   [ipv4_addr_of_tuple (10l,0l,0l,1l)]
  )

let main () =
  eprintf "listening to HTTP on port %d\n" port;
  eprintf "finding the static kv_ro block device\n";
  lwt static = OS.Devices.find_kv_ro "static" >>=
    function
    |None -> Printf.printf "fatal error, static kv_ro not found\n%!"; exit 1
    |Some x -> return x in
  lwt xvda = OS.Devices.find_blkif "51712" >>=
    function
    |None -> Printf.printf "fatal error, VBD 51712 (disk 0 = 'xvda') not found\n%!"; exit 1
    |Some x -> return x in

  let callback = Dispatch.t static in
  let spec = {
    Cohttp_lwt_mirage.Server.callback;
    conn_closed = (fun _ () -> ());
  } in
  Net.Manager.create (fun mgr interface id ->
    let src = None, port in
    let fd = Pcap_mirage.open_device xvda in
    let (_: unit Lwt.t) = Pcap_mirage.start_capture (Net.Manager.get_netif interface) fd in
    Net.Manager.configure interface (`IPv4 ip) >>
    Cohttp_lwt_mirage.listen mgr src spec
  )
