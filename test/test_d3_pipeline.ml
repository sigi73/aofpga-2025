open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module DUT = Aofpga_2025.D3.Pipeline
module Harness = Cyclesim_harness.Make (DUT.I) (DUT.O)

let ( <--. ) = Bits.( <--. )

let simple_testbench (sim : Harness.Sim.t) ~(ascii_input : string) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle ?n () = Cyclesim.cycle ?n sim in
  let feed_input c =
    inputs.ascii_input.value <--. c;
    inputs.ascii_input.valid := Bits.vdd;
    cycle ()
  in
  let print () =
    if Bits.to_bool !(outputs.sum_out.valid)
    then (
      let result = Bits.to_unsigned_int !(outputs.sum_out.value) in
      Printf.printf "%d\n" result)
  in
  (* Reset the design *)
  inputs.reset := Bits.vdd;
  cycle ();
  inputs.reset := Bits.gnd;
  cycle ();
  String.iter ascii_input ~f:(fun c ->
    feed_input (int_of_char c);
    print ());
  inputs.ascii_input.valid := Bits.gnd;
  let cycle_and_print ?(n = 1) () =
    for _ = 1 to n do
      cycle ();
      print ()
    done
  in
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
  Harness.run_advanced
    ~waves_config:waves_config_no_waves
    ~create:create_fn
    (simple_testbench ~ascii_input:"1231\n\n");
  [%expect {| 31 |}]
;;

let%expect_test "4in-2out Test 2" =
  let config : DUT.config = { input_length = 4; output_length = 2 } in
  let create_fn = DUT.hierarchical ~config in
  Harness.run_advanced
    ~waves_config:waves_config_no_waves
    ~create:create_fn
    (simple_testbench ~ascii_input:"9879\n\n");
  [%expect {| 99 |}]
;;

let%expect_test "4-in-2out x2" =
  let config : DUT.config = { input_length = 4; output_length = 2 } in
  let create_fn = DUT.hierarchical ~config in
  Harness.run_advanced
    ~waves_config:waves_config_no_waves
    ~create:create_fn
    (simple_testbench ~ascii_input:"1231\n9879\n\n");
  (* Should output running sum *)
  [%expect {| 130 |}]
;;

let%expect_test "P1 Sample" =
  let config : DUT.config = { input_length = 15; output_length = 2 } in
  let create_fn = DUT.hierarchical ~config in
  Harness.run_advanced
    ~waves_config:waves_config_no_waves
    ~create:create_fn
    (simple_testbench
       ~ascii_input:
         "987654321111111\n811111111111119\n234234234234278\n818181911112111\n\n");
  [%expect {| 357 |}]
;;

let%expect_test "P2 Sample" =
  let config : DUT.config = { input_length = 15; output_length = 12 } in
  let create_fn = DUT.hierarchical ~config in
  Harness.run_advanced
    ~waves_config:waves_config_no_waves
    ~create:create_fn
    (simple_testbench
       ~ascii_input:
         "987654321111111\n811111111111119\n234234234234278\n818181911112111\n\n");
  [%expect {| 3121910778619 |}]
;;

let%expect_test "P1" =
  let config : DUT.config = { input_length = 100; output_length = 2 } in
  let create_fn = DUT.hierarchical ~config in
  let ascii_input = In_channel.read_all "aoc-inputs/d3.txt" in
  Harness.run_advanced
    ~waves_config:waves_config_no_waves
    ~create:create_fn
    (simple_testbench ~ascii_input);
  [%expect {| 17524 |}]
;;

let%expect_test "P2" =
  let config : DUT.config = { input_length = 100; output_length = 12 } in
  let create_fn = DUT.hierarchical ~config in
  let ascii_input = In_channel.read_all "aoc-inputs/d3.txt" in
  Harness.run_advanced
    ~waves_config:waves_config_no_waves
    ~create:create_fn
    (simple_testbench ~ascii_input);
  [%expect {| 173848577117276 |}]
;;
