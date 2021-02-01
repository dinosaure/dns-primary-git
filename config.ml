open Mirage

type mimic = Mimic

let mimic = typ Mimic

let mimic_conf () =
  let packages = [ package "mimic" ] in
  impl @@ object
       inherit base_configurable
       method ty = mimic @-> mimic @-> mimic
       method module_name = "Mimic.Merge"
       method! packages = Key.pure packages
       method name = "merge_ctx"
       method! connect _ _modname =
         function
         | [ a; b ] -> Fmt.str "Lwt.return (Mimic.merge %s %s)" a b
         | [ x ] -> Fmt.str "%s.ctx" x
         | _ -> Fmt.str "Lwt.return Mimic.empty"
     end

let merge ctx0 ctx1 = mimic_conf () $ ctx0 $ ctx1

let mimic_tcp_conf ~edn () =
  let packages = [ package "git-mirage" ~sublibs:[ "tcp" ] ] in
  let edn = Key.abstract edn in
  impl @@ object
       inherit base_configurable
       method ty = stackv4 @-> mimic
       method! keys = [ edn ]
       method module_name = "Git_mirage_tcp.Make"
       method! packages = Key.pure packages
       method name = "tcp_ctx"
       method! connect _ modname =
         function
         | [ stack ] ->
             Fmt.str
               {|let tcp_ctx0 = %s.with_stack %s %s.ctx in
                 Lwt.return tcp_ctx0|}
               modname stack modname
         | _ -> Fmt.str "Lwt.return %s.ctx" modname
     end

let mimic_tcp_impl ~edn stackv4 = mimic_tcp_conf ~edn () $ stackv4

let mimic_git_conf ~edn () =
  let packages = [ package "git-mirage" ] in
  let edn = Key.abstract edn in
  impl @@ object
       inherit base_configurable
       method ty = stackv4 @-> mimic @-> mimic
       method! keys = [ edn ]
       method module_name = "Git_mirage.Make"
       method! packages = Key.pure packages
       method name = "git_ctx"
       method! connect _ modname = function
         | [ _stackv4; ctx; ] ->
           Fmt.str
             {|let git_ctx1 = %s.with_smart_git_endpoint (%a) %s in
               let git_ctx2 = %s.with_resolv git_ctx1 in
               Lwt.return git_ctx2|}
             modname Key.serialize_call edn ctx
             modname
         | _ -> assert false
     end

let mimic_git_impl ~edn stackv4 mimic_tcp =
  mimic_git_conf ~edn () $ stackv4 $ mimic_tcp

let mimic_ssh_conf ~edn ~kind ~seed ~auth () =
  let seed = Key.abstract seed in
  let auth = Key.abstract auth in
  let edn = Key.abstract edn in
  let packages = [ package "git-mirage" ~sublibs:[ "ssh" ] ] in
  impl @@ object
       inherit base_configurable
       method ty = stackv4 @-> mimic @-> mimic @-> mclock @-> mimic
       method! keys = [ seed; auth; edn ]
       method module_name = "Git_mirage_ssh.Make"
       method! packages = Key.pure packages
       method name = "ssh_ctx"
       method! connect _ modname =
         function
         | [ _; tcp_ctx; git_ctx; _ ] ->
             let with_key =
               match kind with
               | `Rsa -> "with_rsa_key"
               | `Ed25519 -> "with_ed25519_key"
             in
             Fmt.str
               {| let ctx00 = Mimic.merge %s %s in
             let ctx01 = Option.fold ~none:ctx00 ~some:(fun v -> %s.%s v ctx00) %a in
             let ctx02 = Option.fold ~none:ctx01 ~some:(fun v -> %s.with_authenticator v ctx01) %a in
             let ctx03 = %s.with_resolv ctx02 in
             Lwt.return (%s.with_resolv (%s.with_smart_git_endpoint (%a) ctx03)) |}
               tcp_ctx git_ctx modname with_key Key.serialize_call seed modname
               Key.serialize_call auth modname modname modname
               Key.serialize_call edn
         | _ -> assert false
     end

let mimic_ssh_impl ~edn ~kind ~seed ~auth stackv4 mimic_tcp mimic_git mclock =
  mimic_ssh_conf ~edn ~kind ~seed ~auth ()
  $ stackv4
  $ mimic_tcp
  $ mimic_git
  $ mclock

let mimic_dns_conf ~edn () =
  let packages = [ package "git-mirage" ~sublibs:[ "dns" ] ] in
  let edn = Key.abstract edn in
  impl @@ object
       inherit base_configurable
       method ty = random @-> mclock @-> time @-> stackv4 @-> mimic @-> mimic
       method! keys = [ edn ]
       method module_name = "Git_mirage_dns.Make"
       method! packages = Key.pure packages
       method name = "dns_ctx"
       method! connect _ modname =
         function
         | [ _; _; _; _; ctx ] ->
             Fmt.str
               "Lwt.return (%s.with_resolv (%s.with_smart_git_endpoint %a %s))"
               modname modname Key.serialize_call edn ctx
         | _ -> assert false
     end

let mimic_dns_impl ~edn random mclock time stackv4 mimic_tcp =
  mimic_dns_conf ~edn ()
  $ random
  $ mclock
  $ time
  $ stackv4
  $ mimic_tcp

(* / *)

let remote_k =
  let doc = Key.Arg.info ~doc:"Remote git repository." ["r"; "remote"] in
  Key.(create "remote" Arg.(opt string "git@localhost:mirage/zone" doc))

let axfr =
  let doc = Key.Arg.info ~doc:"Allow unauthenticated zone transfer." ["axfr"] in
  Key.(create "axfr" Arg.(flag doc))

let ssh_seed =
  let doc = Key.Arg.info ~doc:"Seed of the private SSH key." [ "ssh-seed" ] in
  Key.(create "ssh_seed" Arg.(opt (some string) None doc))

let ssh_auth =
  let doc = Key.Arg.info ~doc:"SSH public key of the remote Git endpoint." [ "ssh-auth" ] in
  Key.(create "ssh_auth" Arg.(opt (some string) None doc))

let with_dns_resolver =
  let doc = Key.Arg.info ~doc:"Use a DNS resolver for HTTPS Git remote endpoint." [ "with-dns-resolver" ] in
  Key.(create "with_dns_resolver" Arg.(flag ~stage:`Configure doc))

let dns_handler =
  let packages = [
    package "logs" ;
    package ~min:"4.3.0" ~sublibs:["mirage"; "zone"] "dns-server";
    package "dns-tsig";
    package ~min:"2.3.0" "irmin-mirage";
    package ~min:"2.3.0" "irmin-mirage-git";
    package ~min:"3.1.0" "git";
    package "conduit-mirage";
  ] in
  foreign
    ~keys:[Key.abstract remote_k ; Key.abstract axfr; Key.abstract with_dns_resolver]
    ~packages
    "Unikernel.Main"
    (random @-> pclock @-> mclock @-> time @-> stackv4 @-> mimic @-> job)

let mimic ~edn ~kind ~seed ~auth random time stackv4 mclock () =
  let mtcp = mimic_tcp_impl ~edn stackv4 in
  let mgit = mimic_git_impl ~edn stackv4 mtcp in
  let mssh = mimic_ssh_impl ~edn ~kind ~seed ~auth stackv4 mtcp mgit mclock in
  (* if_impl Key.(value with_dns_resolver)
    (merge (mimic_dns_impl ~edn random mclock time stackv4 mtcp) mssh) *)
    mssh

let () =
  let net = generic_stackv4 default_network in
  let random = default_random in
  let pclock = default_posix_clock in
  let mclock = default_monotonic_clock in
  let time = default_time in
  let mimic = mimic ~edn:remote_k ~kind:`Rsa ~seed:ssh_seed ~auth:ssh_auth random time net mclock in
  register "primary-git"
    [ dns_handler $ random $ pclock $ mclock $ time $ net $ mimic () ]
