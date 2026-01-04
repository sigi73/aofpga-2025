open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module DUT = Aofpga_2025.D3.Pipeline
module Harness = Cyclesim_harness.Make (DUT.I) (DUT.O)

let ( <--. ) = Bits.( <--. )

type command =
  | Sequence of string (* TODO validate that SEQUENCE is only numbers and \n *)
  | SetAsciiDelay of int

type command_sequence = command list

let simple_testbench (sim : Harness.Sim.t) ~(commands : command_sequence) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle ?n () = Cyclesim.cycle ?n sim in
  let ascii_delay = { contents = 1 } in
  let print () =
    if Bits.to_bool !(outputs.sum_out.valid)
    then (
      let result = Bits.to_unsigned_int !(outputs.sum_out.value) in
      Printf.printf "%d\n" result)
  in
  let cycle_and_print ?(n = 1) () =
    for _ = 1 to n do
      cycle ();
      print ()
    done
  in
  let feed_input c =
    inputs.ascii_input.value <--. c;
    inputs.ascii_input.valid := Bits.vdd;
    cycle_and_print ();
    if !ascii_delay > 1
    then (
      inputs.ascii_input.valid := Bits.gnd;
      cycle_and_print ~n:(!ascii_delay - 1) ())
  in
  (* Reset the design *)
  inputs.reset := Bits.vdd;
  cycle ();
  inputs.reset := Bits.gnd;
  cycle ();
  List.iter commands ~f:(fun cmd ->
    match cmd with
    | SetAsciiDelay d ->
      assert (d >= 1);
      ascii_delay := d
    | Sequence s -> String.iter s ~f:(fun c -> feed_input (int_of_char c)));
  inputs.ascii_input.valid := Bits.gnd;
  (* Flush everything through the pipeline. TODO: Parameterize this? *)
  cycle_and_print ~n:100 ()
;;

let waves_config_no_waves = Waves_config.no_waves

let waves_config_hardcaml =
  Waves_config.to_directory "/tmp/"
  |> Waves_config.as_wavefile_format ~format:Hardcamlwaveform
;;

let waves_config_vcd =
  Waves_config.to_directory "/tmp/" |> Waves_config.as_wavefile_format ~format:Vcd
;;

let%expect_test "4in-2out Test 1" =
  let config : DUT.config = { input_length = 4; output_length = 2 } in
  let create_fn = DUT.hierarchical ~config in
  let ascii_delay_tests = [ 1; 2; 3; 4; 8; 16; 30 ] in
  List.iter ascii_delay_tests ~f:(fun delay ->
    Printf.printf "Delay: %d\n" delay;
    let commands = [ SetAsciiDelay delay; Sequence "1231\n\n" ] in
    Harness.run_advanced
      ~waves_config:waves_config_no_waves
      ~create:create_fn
      (simple_testbench ~commands));
  [%expect
    {|
  Delay: 1
  31
  Delay: 2
  31
  Delay: 3
  31
  Delay: 4
  31
  Delay: 8
  31
  Delay: 16
  31
  Delay: 30
  31
    |}]
;;

let%expect_test "4in-2out Test 2" =
  let config : DUT.config = { input_length = 4; output_length = 2 } in
  let create_fn = DUT.hierarchical ~config in
  let ascii_delay_tests = [ 1; 2; 3; 4; 8; 16; 30 ] in
  List.iter ascii_delay_tests ~f:(fun delay ->
    Printf.printf "Delay: %d\n" delay;
    let commands = [ SetAsciiDelay delay; Sequence "9879\n\n" ] in
    Harness.run_advanced
      ~waves_config:waves_config_no_waves
      ~create:create_fn
      (simple_testbench ~commands));
  [%expect
    {|
  Delay: 1
  99
  Delay: 2
  99
  Delay: 3
  99
  Delay: 4
  99
  Delay: 8
  99
  Delay: 16
  99
  Delay: 30
  99
    |}]
;;

let%expect_test "4in-2out x2" =
  let config : DUT.config = { input_length = 4; output_length = 2 } in
  let create_fn = DUT.hierarchical ~config in
  let ascii_delay_tests = [ 1; 2; 3; 4; 8; 16; 30 ] in
  List.iter ascii_delay_tests ~f:(fun delay ->
    Printf.printf "Delay: %d\n" delay;
    let commands = [ SetAsciiDelay delay; Sequence "1231\n9879\n\n" ] in
    Harness.run_advanced
      ~waves_config:waves_config_no_waves
      ~create:create_fn
      (simple_testbench ~commands));
  [%expect
    {|
  Delay: 1
  130
  Delay: 2
  130
  Delay: 3
  130
  Delay: 4
  130
  Delay: 8
  130
  Delay: 16
  130
  Delay: 30
  130
    |}]
;;

let%expect_test "P1 Sample" =
  let config : DUT.config = { input_length = 15; output_length = 2 } in
  let create_fn = DUT.hierarchical ~config in
  let ascii_delay_tests = [ 1; 2; 3; 4; 8; 16; 30 ] in
  List.iter ascii_delay_tests ~f:(fun delay ->
    Printf.printf "Delay: %d\n" delay;
    let commands =
      [ SetAsciiDelay delay
      ; Sequence "987654321111111\n811111111111119\n234234234234278\n818181911112111\n\n"
      ]
    in
    Harness.run_advanced
      ~waves_config:waves_config_no_waves
      ~create:create_fn
      (simple_testbench ~commands));
  [%expect
    {|
    Delay: 1
    357
    Delay: 2
    357
    Delay: 3
    357
    Delay: 4
    357
    Delay: 8
    357
    Delay: 16
    357
    Delay: 30
    357
    |}]
;;

let%expect_test "P2 Sample" =
  let config : DUT.config = { input_length = 15; output_length = 12 } in
  let create_fn = DUT.hierarchical ~config in
  let ascii_delay_tests = [ 1; 2; 3; 4; 8; 16; 30 ] in
  List.iter ascii_delay_tests ~f:(fun delay ->
    Printf.printf "Delay: %d\n" delay;
    let commands =
      [ SetAsciiDelay delay
      ; Sequence "987654321111111\n811111111111119\n234234234234278\n818181911112111\n\n"
      ]
    in
    Harness.run_advanced
      ~waves_config:waves_config_no_waves
      ~create:create_fn
      (simple_testbench ~commands));
  [%expect
    {|
    Delay: 1
    3121910778619
    Delay: 2
    3121910778619
    Delay: 3
    3121910778619
    Delay: 4
    3121910778619
    Delay: 8
    3121910778619
    Delay: 16
    3121910778619
    Delay: 30
    3121910778619
    |}]
;;

let%expect_test "P1" =
  let config : DUT.config = { input_length = 100; output_length = 2 } in
  let create_fn = DUT.hierarchical ~config in
  let ascii_input = In_channel.read_all "aoc-inputs/d3.txt" in
  let ascii_delay_tests = [ 1; 2; 3; 4; 8; 16; 30 ] in
  List.iter ascii_delay_tests ~f:(fun delay ->
    Printf.printf "Delay: %d\n" delay;
    let commands = [ SetAsciiDelay delay; Sequence ascii_input ] in
    Harness.run_advanced
      ~waves_config:waves_config_no_waves
      ~create:create_fn
      (simple_testbench ~commands));
  [%expect
    {|
    Delay: 1
    17524
    Delay: 2
    17524
    Delay: 3
    17524
    Delay: 4
    17524
    Delay: 8
    17524
    Delay: 16
    17524
    Delay: 30
    17524
    |}]
;;

let%expect_test "P2" =
  let config : DUT.config = { input_length = 100; output_length = 12 } in
  let create_fn = DUT.hierarchical ~config in
  let ascii_input = In_channel.read_all "aoc-inputs/d3.txt" in
  let ascii_delay_tests = [ 1; 2; 3; 4; 8; 16; 30 ] in
  List.iter ascii_delay_tests ~f:(fun delay ->
    Printf.printf "Delay: %d\n" delay;
    let commands = [ SetAsciiDelay delay; Sequence ascii_input ] in
    Harness.run_advanced
      ~waves_config:waves_config_no_waves
      ~create:create_fn
      (simple_testbench ~commands));
  [%expect
    {|
    Delay: 1
    173848577117276
    Delay: 2
    173848577117276
    Delay: 3
    173848577117276
    Delay: 4
    173848577117276
    Delay: 8
    173848577117276
    Delay: 16
    173848577117276
    Delay: 30
    173848577117276
    |}]
;;

(* Make sure there is no issue with the pipelining on our feedback *)
let%expect_test "Variable delay" =
  let config : DUT.config = { input_length = 4; output_length = 2 } in
  let create_fn = DUT.hierarchical ~config in
  let commands =
    [ SetAsciiDelay 30
    ; Sequence "123"
    ; SetAsciiDelay 1
    ; Sequence "1\n9"
    ; SetAsciiDelay 30
    ; Sequence "87"
    ; SetAsciiDelay 1
    ; Sequence "9\n\n"
    ]
  in
  Harness.run_advanced
    ~waves_config:waves_config_no_waves
    ~create:create_fn
    (simple_testbench ~commands);
  [%expect {|
  130
    |}]
;;
