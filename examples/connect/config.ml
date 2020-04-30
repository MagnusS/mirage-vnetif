open Mirage

let main =
    let packages = [
            package "mirage-vnetif";
            package "mirage-net-unix";
            package "mirage-net";
            package "mirage-unix";
            package "mirage-clock-unix";
            package "mirage-types";
            package "mirage-stack";
            package "mirage-protocols";
            package "mirage-clock";
            package "arp";
            package "arp-mirage";
            package "tcpip";
            package ~sublibs:["stack-direct"; "ipv4"; "tcp"; "udp"; "icmpv4"] "tcpip" ] in
    foreign
      ~packages
      "Unikernel.Main" (console @-> random @-> job)

let () =
  register "unikernel" [
    main $ default_console $ default_random
  ]
