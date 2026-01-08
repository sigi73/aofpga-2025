open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module DUT = Aofpga_2025.D3.TopWithoutUart
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
  let print () =
    if Bits.to_bool !(outputs.ascii_output.valid)
    then (
      let result = Bits.to_char !(outputs.ascii_output.value) in
      Printf.printf "%c" result)
  in
  let cycle_and_print ?(n = 1) () =
    for _ = 1 to n do
      cycle ();
      print ()
    done
  in
  let ascii_delay = { contents = 1 } in
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
  (* Flush everything through the pipeline *)
  cycle_and_print ~n:1000 ()
;;

let simple_testbench_fixed_delay (sim : Harness.Sim.t) ~delay ~(ascii_input : string) =
  let command_sequence = [ SetAsciiDelay delay; Sequence ascii_input ] in
  simple_testbench sim ~commands:command_sequence
;;

let simple_testbench_fixed_delays (sim : Harness.Sim.t) ~delays ~(ascii_input : string) =
  List.iter delays ~f:(fun delay -> simple_testbench_fixed_delay sim ~delay ~ascii_input)
;;

let waves_config_no_waves = Waves_config.no_waves

let waves_config_hardcaml =
  Waves_config.to_directory "/tmp/"
  |> Waves_config.as_wavefile_format ~format:Hardcamlwaveform
;;

let waves_config_vcd =
  Waves_config.to_directory "/tmp/" |> Waves_config.as_wavefile_format ~format:Vcd
;;

let delays = [ 1; 2; 3; 4; 8; 16; 30 ]

let%expect_test "4in-2out Test 1" =
  let config : DUT.config =
    { input_length = 4; p1_output_length = 2; p2_output_length = 3 }
  in
  let create_fn = DUT.hierarchical ~config in
  Harness.run_advanced
    ~waves_config:waves_config_no_waves
    ~create:create_fn
    (simple_testbench_fixed_delays ~delays ~ascii_input:"1231\n\n");
  [%expect
    {|
    Part 1: 31
    Part 2: 231
    Part 1: 31
    Part 2: 231
    Part 1: 31
    Part 2: 231
    Part 1: 31
    Part 2: 231
    Part 1: 31
    Part 2: 231
    Part 1: 31
    Part 2: 231
    Part 1: 31
    Part 2: 231
    |}]
;;

let%expect_test "4in-2out Test 2" =
  let config : DUT.config =
    { input_length = 4; p1_output_length = 2; p2_output_length = 3 }
  in
  let create_fn = DUT.hierarchical ~config in
  Harness.run_advanced
    ~waves_config:waves_config_no_waves
    ~create:create_fn
    (simple_testbench_fixed_delays ~delays ~ascii_input:"9879\n\n");
  [%expect
    {|
    Part 1: 99
    Part 2: 989
    Part 1: 99
    Part 2: 989
    Part 1: 99
    Part 2: 989
    Part 1: 99
    Part 2: 989
    Part 1: 99
    Part 2: 989
    Part 1: 99
    Part 2: 989
    Part 1: 99
    Part 2: 989
    |}]
;;

let%expect_test "4-in-2out x2" =
  let config : DUT.config =
    { input_length = 4; p1_output_length = 2; p2_output_length = 3 }
  in
  let create_fn = DUT.hierarchical ~config in
  Harness.run_advanced
    ~waves_config:waves_config_no_waves
    ~create:create_fn
    (simple_testbench_fixed_delays ~delays ~ascii_input:"1231\n9879\n\n");
  [%expect
    {|
    Part 1: 130
    Part 2: 1220
    Part 1: 130
    Part 2: 1220
    Part 1: 130
    Part 2: 1220
    Part 1: 130
    Part 2: 1220
    Part 1: 130
    Part 2: 1220
    Part 1: 130
    Part 2: 1220
    Part 1: 130
    Part 2: 1220
    |}]
;;

let%expect_test "Sample" =
  let config : DUT.config =
    { input_length = 15; p1_output_length = 2; p2_output_length = 12 }
  in
  let create_fn = DUT.hierarchical ~config in
  Harness.run_advanced
    ~waves_config:waves_config_no_waves
    ~create:create_fn
    (simple_testbench_fixed_delays
       ~delays
       ~ascii_input:
         "987654321111111\n811111111111119\n234234234234278\n818181911112111\n\n");
  [%expect
    {|
    Part 1: 357
    Part 2: 3121910778619
    Part 1: 357
    Part 2: 3121910778619
    Part 1: 357
    Part 2: 3121910778619
    Part 1: 357
    Part 2: 3121910778619
    Part 1: 357
    Part 2: 3121910778619
    Part 1: 357
    Part 2: 3121910778619
    Part 1: 357
    Part 2: 3121910778619
    |}]
;;

let%expect_test "Full input" =
  let config : DUT.config =
    { input_length = 100; p1_output_length = 2; p2_output_length = 12 }
  in
  let create_fn = DUT.hierarchical ~config in
  let ascii_input =
    let s = In_channel.read_all "aoc-inputs/d3.txt" in
    if String.is_suffix s ~suffix:"\n\n"
    then s
    else if String.is_suffix s ~suffix:"\n"
    then s ^ "\n"
    else s ^ "\n\n"
  in
  Harness.run_advanced
    ~waves_config:waves_config_no_waves
    ~create:create_fn
    (simple_testbench_fixed_delays ~delays ~ascii_input);
  [%expect
    {|
    Part 1: 17524
    Part 2: 173848577117276
    Part 1: 17524
    Part 2: 173848577117276
    Part 1: 17524
    Part 2: 173848577117276
    Part 1: 17524
    Part 2: 173848577117276
    Part 1: 17524
    Part 2: 173848577117276
    Part 1: 17524
    Part 2: 173848577117276
    Part 1: 17524
    Part 2: 173848577117276
    |}]
;;

let%expect_test "Large input" =
  (* 15 is about the max for our 60 bit sum bus (1000 15 digit numbers fits in 60 bits) *)
  let config : DUT.config =
    { input_length = 1000; p1_output_length = 12; p2_output_length = 15 }
  in
  let create_fn = DUT.hierarchical ~config in
  let ascii_input =
    let s = In_channel.read_all "aoc-inputs/big_input_d3.txt" in
    if String.is_suffix s ~suffix:"\n\n"
    then s
    else if String.is_suffix s ~suffix:"\n"
    then s ^ "\n"
    else s ^ "\n\n"
  in
  Harness.run_advanced
    ~waves_config:waves_config_no_waves
    ~create:create_fn
    (simple_testbench_fixed_delays ~delays:[ 1 ] ~ascii_input);
  (* I think these look like this because we add a bunch of stuff with most significant digit of 9/high number? Weird, something to think about. Verified with my normal AoC solution *)
  [%expect {|
    Part 1: 999999999999000
    Part 2: 999999999999999000
    |}]
;;
