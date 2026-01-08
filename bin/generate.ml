open! Core
open! Hardcaml
open! Aofpga_2025

let generate_d3_rtl () =
  let module C = Circuit.With_interface (D3.TopWithoutUart.I) (D3.TopWithoutUart.O) in
  let scope = Scope.create ~auto_label_hierarchical_ports:true () in
  let circuit =
    C.create_exn
      ~name:"d3_pipeline_top_no_uart"
      (D3.TopWithoutUart.hierarchical
         ~config:{ input_length = 100; p1_output_length = 2; p2_output_length = 12 }
         scope)
  in
  let rtl_circuits =
    Rtl.create ~database:(Scope.circuit_database scope) Verilog [ circuit ]
  in
  let rtl = Rtl.full_hierarchy rtl_circuits |> Rope.to_string in
  print_endline rtl
;;

let d3_rtl_command =
  Command.basic
    ~summary:""
    [%map_open.Command
      let () = return () in
      fun () -> generate_d3_rtl ()]
;;

let () = Command_unix.run (Command.group ~summary:"" [ "d3", d3_rtl_command ])
