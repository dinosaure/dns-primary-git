(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Mirage

let remote_k =
  let doc = Key.Arg.info ~doc:"Remote git repository." ["r"; "remote"] in
  Key.(create "remote" Arg.(opt string "https://github.com/roburio/udns.git" doc))

let axfr =
  let doc = Key.Arg.info ~doc:"Allow unauthenticated zone transfer." ["axfr"] in
  Key.(create "axfr" Arg.(flag doc))

let ssh_seed =
  let doc = Key.Arg.info ~doc:"Seed of the private SSH key" [ "ssh-seed" ] in
  Key.(create "ssh-seed" Arg.(opt (some string) None doc))

let ssh_auth =
  let doc = Key.Arg.info ~doc:"SSH Public key of the remote Git endpoint" [ "ssh-auth" ] in
  Key.(create "ssh-auth" Arg.(opt (some string) None doc))

let dns_handler =
  let packages = [
    package "logs" ;
    package ~min:"4.3.0" ~sublibs:["mirage"; "zone"] "dns-server";
    package "dns-tsig";
    package ~min:"2.0.0" "irmin-mirage";
    package ~min:"2.0.0" "irmin-mirage-git";
    package ~min:"3.0.0" ~sublibs:[ "tcp" ] "conduit-mirage";
    package "awa-conduit";
  ] in
  foreign
    ~keys:[ Key.abstract remote_k
          ; Key.abstract axfr
          ; Key.abstract ssh_seed
          ; Key.abstract ssh_auth ]
    ~packages
    "Unikernel.Main"
    (random @-> pclock @-> mclock @-> time @-> stackv4 @-> job)

let () =
  let net = generic_stackv4 default_network in
  register "primary-git"
    [dns_handler $ default_random $ default_posix_clock $ default_monotonic_clock $
     default_time $ net ]
