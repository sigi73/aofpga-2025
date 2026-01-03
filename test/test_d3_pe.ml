open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module DUT = Aofpga_2025.D3.PE
module Harness = Cyclesim_harness.Make (DUT.I) (DUT.O)

let ( <--. ) = Bits.( <--. )

type digit =
  | D1
  | D2
  | D3
  | D4
  | D5
  | D6
  | D7
  | D8
  | D9

let int_of_digit = function
  | D1 -> 1
  | D2 -> 2
  | D3 -> 3
  | D4 -> 4
  | D5 -> 5
  | D6 -> 6
  | D7 -> 7
  | D8 -> 8
  | D9 -> 9
;;

type command =
  | ChipReset (* Performs chip reset *)
  | SequenceReset (* Resets the input sequence and flush *)
  | Flush (* Sends a flush *)
  | Input of digit
  | SetSum of int

type command_sequence = command list

let simple_testbench (sim : Harness.Sim.t) ~(commands : command_sequence) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle ?n () = Cyclesim.cycle ?n sim in
  let input_sum = { contents = 0 } in
  let last_running_sum_valid = { contents = false } in
  let feed_input n idx =
    inputs.pe_in.data.value <--. n;
    inputs.pe_in.data.valid := Bits.vdd;
    inputs.pe_in.index.value <--. idx;
    inputs.pe_in.index.valid := Bits.vdd;
    inputs.pe_in.flush := Bits.gnd;
    inputs.pe_in.running_sum.value <--. !input_sum;
    inputs.pe_in.running_sum.valid := Bits.vdd
  in
  let flush idx =
    inputs.pe_in.flush := Bits.vdd;
    inputs.pe_in.data.valid := Bits.gnd;
    inputs.pe_in.index.value <--. idx;
    inputs.pe_in.index.valid := Bits.vdd
  in
  let current_index = { contents = 0 } in
  let print () =
    if Bits.to_bool !(outputs.pe_out.running_sum.valid)
    then (
      if not !last_running_sum_valid
      then (
        last_running_sum_valid := true;
        let result = Bits.to_unsigned_int !(outputs.pe_out.running_sum.value) in
        Printf.printf "%d\n" result))
    else last_running_sum_valid := false
  in
  List.iter commands ~f:(fun cmd ->
    (match cmd with
     | ChipReset ->
       inputs.reset := Bits.vdd;
       cycle ();
       inputs.reset := Bits.gnd;
       cycle ()
     | SequenceReset -> current_index := 0
     | Flush ->
       flush !current_index;
       cycle ();
       current_index := !current_index + 1
     | Input d ->
       let n = int_of_digit d in
       feed_input n !current_index;
       cycle ();
       current_index := !current_index + 1
     | SetSum n -> input_sum := n);
    (* For now just print out whenever we have a sol for this index*)
    (* TODO: Implement checking for the pe_out output as well*)
    print ());
  let cycle_and_print ?(n = 1) () =
    for _ = 1 to n do
      cycle ();
      print ()
    done
  in
  cycle_and_print ~n:10 ()
;;

let waves_config_no_waves = Waves_config.no_waves

let waves_config_hardcaml =
  Waves_config.to_directory "/tmp/"
  |> Waves_config.as_wavefile_format ~format:Hardcamlwaveform
;;

let%expect_test "Basic Test" =
  let config : DUT.config =
    { min_index = 0; max_index = 3; multiply_digit_by = 1; last_input_index = 4 }
  in
  let commands =
    [ ChipReset
    ; Input D3 (* Index 0 *)
    ; Input D1 (* Index 1 *)
    ; Flush (* Index 2 *)
    ; Input D2 (* Index 3 *)
    ; Input D3 (* Index 4 *)
    ; SequenceReset (* Reset sequence to start over *)
    ; Input D5 (* Index 0 *)
    ; Input D6 (* Index 1 *)
    ; Input D2 (* Index 2 *)
    ; Input D3 (* Index 3 *)
    ; Input D7 (* Index 4 *)
    ]
  in
  let create_fn = DUT.hierarchical ~config in
  Harness.run_advanced
    ~waves_config:waves_config_no_waves
    ~create:create_fn
    (simple_testbench ~commands);
  [%expect {|
    2
    6
    |}]
;;

let%expect_test "Basic Test With Multiply" =
  let config : DUT.config =
    { min_index = 0; max_index = 3; multiply_digit_by = 10; last_input_index = 4 }
  in
  let commands =
    [ ChipReset
    ; Input D3 (* Index 0 *)
    ; Input D1 (* Index 1 *)
    ; Flush (* Index 2 *)
    ; Input D2 (* Index 3 *)
    ; Input D3 (* Index 4 *)
    ; SequenceReset (* Reset sequence to start over *)
    ; Input D5 (* Index 0 *)
    ; Input D6 (* Index 1 *)
    ; Input D2 (* Index 2 *)
    ; Input D3 (* Index 3 *)
    ; Input D7 (* Index 4 *)
    ]
  in
  let create_fn = DUT.hierarchical ~config in
  Harness.run_advanced
    ~waves_config:waves_config_no_waves
    ~create:create_fn
    (simple_testbench ~commands);
  [%expect {|
    20
    60
    |}]
;;

let%expect_test "Basic Test With Initial Sum And Multiply" =
  let config : DUT.config =
    { min_index = 0; max_index = 3; multiply_digit_by = 10; last_input_index = 4 }
  in
  let commands =
    [ ChipReset
    ; SetSum 700
    ; Input D3 (* Index 0 *)
    ; Input D1 (* Index 1 *)
    ; Flush (* Index 2 *)
    ; Input D2 (* Index 3 *)
    ; Input D3 (* Index 4 *)
    ; SequenceReset (* Reset sequence to start over *)
    ; Input D5 (* Index 0 *)
    ; Input D6 (* Index 1 *)
    ; Input D2 (* Index 2 *)
    ; Input D3 (* Index 3 *)
    ; Input D7 (* Index 4 *)
    ]
  in
  let create_fn = DUT.hierarchical ~config in
  Harness.run_advanced
    ~waves_config:waves_config_no_waves
    ~create:create_fn
    (simple_testbench ~commands);
  [%expect {|
    720
    760
    |}]
;;
