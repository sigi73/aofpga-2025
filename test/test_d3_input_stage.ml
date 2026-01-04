open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module DUT = Aofpga_2025.D3.InputStage
module Harness = Cyclesim_harness.Make (DUT.I) (DUT.O)
(* module Range_finder = Aofpga_2025.Range_finder *)
(* module Harness = Cyclesim_harness.Make (Range_finder.I) (Range_finder.O) *)

let ( <--. ) = Bits.( <--. )

let simple_testbench ?(ascii_delay = 1) (sim : Harness.Sim.t) ~(ascii_input : string) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle ?n () = Cyclesim.cycle ?n sim in
  let print () =
    printf
      "Data: %d (Valid: %b) Index: %d (Valid: %b) Flush: %b InputIsDone: %b\n"
      (Bits.to_int_trunc !(outputs.pe_out.data.value))
      (Bits.to_bool !(outputs.pe_out.data.valid))
      (Bits.to_int_trunc !(outputs.pe_out.index.value))
      (Bits.to_bool !(outputs.pe_out.index.valid))
      (Bits.to_bool !(outputs.pe_out.flush))
      (Bits.to_bool !(outputs.pe_out.input_is_done))
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
    if ascii_delay > 1
    then (
      inputs.ascii_input.valid := Bits.gnd;
      cycle_and_print ~n:(ascii_delay - 1) ())
  in
  (* Reset the design *)
  inputs.reset := Bits.vdd;
  cycle ();
  inputs.reset := Bits.gnd;
  cycle_and_print ();
  (* A few cycles to show no valid output *)
  cycle_and_print ();
  cycle_and_print ();
  cycle_and_print ();
  String.iter ascii_input ~f:(fun c -> feed_input (int_of_char c));
  inputs.ascii_input.valid := Bits.gnd;
  (* A few cycles to show no valid output *)
  cycle_and_print ();
  cycle_and_print ()
;;

let waves_config_no_waves = Waves_config.no_waves

let waves_config_hardcaml =
  Waves_config.to_directory "/tmp/"
  |> Waves_config.as_wavefile_format ~format:Hardcamlwaveform
;;

let%expect_test "Test 1" =
  Harness.run_advanced
    ~waves_config:waves_config_no_waves
    ~create:DUT.hierarchical
    (simple_testbench ~ascii_input:"123\n987\n\n");
  [%expect
    {|
    Data: 0 (Valid: false) Index: 65535 (Valid: false) Flush: false InputIsDone: false
    Data: 0 (Valid: false) Index: 65535 (Valid: false) Flush: false InputIsDone: false
    Data: 0 (Valid: false) Index: 65535 (Valid: false) Flush: false InputIsDone: false
    Data: 0 (Valid: false) Index: 65535 (Valid: false) Flush: false InputIsDone: false
    Data: 1 (Valid: true) Index: 0 (Valid: true) Flush: false InputIsDone: false
    Data: 2 (Valid: true) Index: 1 (Valid: true) Flush: false InputIsDone: false
    Data: 3 (Valid: true) Index: 2 (Valid: true) Flush: false InputIsDone: false
    Data: 3 (Valid: false) Index: 65535 (Valid: false) Flush: true InputIsDone: false
    Data: 9 (Valid: true) Index: 0 (Valid: true) Flush: false InputIsDone: false
    Data: 8 (Valid: true) Index: 1 (Valid: true) Flush: false InputIsDone: false
    Data: 7 (Valid: true) Index: 2 (Valid: true) Flush: false InputIsDone: false
    Data: 7 (Valid: false) Index: 65535 (Valid: false) Flush: true InputIsDone: false
    Data: 7 (Valid: false) Index: 65535 (Valid: false) Flush: false InputIsDone: true
    Data: 7 (Valid: false) Index: 65535 (Valid: false) Flush: false InputIsDone: false
    Data: 7 (Valid: false) Index: 65535 (Valid: false) Flush: false InputIsDone: false
    |}]
;;

let%expect_test "Test 2" =
  Harness.run_advanced
    ~waves_config:waves_config_no_waves
    ~create:DUT.hierarchical
    (simple_testbench ?ascii_delay:(Some 2) ~ascii_input:"123\n987\n\n");
  [%expect
    {|
    Data: 0 (Valid: false) Index: 65535 (Valid: false) Flush: false InputIsDone: false
    Data: 0 (Valid: false) Index: 65535 (Valid: false) Flush: false InputIsDone: false
    Data: 0 (Valid: false) Index: 65535 (Valid: false) Flush: false InputIsDone: false
    Data: 0 (Valid: false) Index: 65535 (Valid: false) Flush: false InputIsDone: false
    Data: 1 (Valid: true) Index: 0 (Valid: true) Flush: false InputIsDone: false
    Data: 1 (Valid: false) Index: 0 (Valid: false) Flush: false InputIsDone: false
    Data: 2 (Valid: true) Index: 1 (Valid: true) Flush: false InputIsDone: false
    Data: 2 (Valid: false) Index: 1 (Valid: false) Flush: false InputIsDone: false
    Data: 3 (Valid: true) Index: 2 (Valid: true) Flush: false InputIsDone: false
    Data: 3 (Valid: false) Index: 2 (Valid: false) Flush: false InputIsDone: false
    Data: 3 (Valid: false) Index: 65535 (Valid: false) Flush: true InputIsDone: false
    Data: 3 (Valid: false) Index: 65535 (Valid: false) Flush: false InputIsDone: false
    Data: 9 (Valid: true) Index: 0 (Valid: true) Flush: false InputIsDone: false
    Data: 9 (Valid: false) Index: 0 (Valid: false) Flush: false InputIsDone: false
    Data: 8 (Valid: true) Index: 1 (Valid: true) Flush: false InputIsDone: false
    Data: 8 (Valid: false) Index: 1 (Valid: false) Flush: false InputIsDone: false
    Data: 7 (Valid: true) Index: 2 (Valid: true) Flush: false InputIsDone: false
    Data: 7 (Valid: false) Index: 2 (Valid: false) Flush: false InputIsDone: false
    Data: 7 (Valid: false) Index: 65535 (Valid: false) Flush: true InputIsDone: false
    Data: 7 (Valid: false) Index: 65535 (Valid: false) Flush: false InputIsDone: false
    Data: 7 (Valid: false) Index: 65535 (Valid: false) Flush: false InputIsDone: true
    Data: 7 (Valid: false) Index: 65535 (Valid: false) Flush: false InputIsDone: false
    Data: 7 (Valid: false) Index: 65535 (Valid: false) Flush: false InputIsDone: false
    Data: 7 (Valid: false) Index: 65535 (Valid: false) Flush: false InputIsDone: false
    |}]
;;
