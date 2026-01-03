open! Core
open! Hardcaml
open! Signal

(* Only need to represent digits 1-9 *)
let data_width = 4
let ascii_bit_width = 8

(* This implementation requires a counter of the index of the input. This puts an upper bound on the length of a single row of input. AoC problem is 100 inputs per line, so 8 is plenty. For larger inputs requires larger size. *)
(* maybe todo: Parameterize this on the input size since we need to know that anyway?*)
let index_width = 16

(* AOC P2 adds up 200 12 digit numbers. Should fit within 48 bits. We use 60 as that is the input size of the Print_decimal_outputs module. *)
let sum_width = 60

(* The inputs and outputs for adjacent processing elements. *)
module PeInterface = struct
  type 'a t =
    { data : 'a With_valid.t [@bits data_width]
    ; index : 'a With_valid.t [@bits index_width]
    ; running_sum : 'a With_valid.t [@bits sum_width]
    ; flush : 'a
    ; input_is_done : 'a
    }
  [@@deriving hardcaml]
end

module PE = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; reset : 'a
      ; pe_in : 'a PeInterface.t
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { pe_out : 'a PeInterface.t } [@@deriving hardcaml]
  end

  type config =
    { min_index : int
    ; max_index : int
    ; last_input_index : int
    ; multiply_digit_by : int
    }

  let can_handle_index (c : config) (index : (Signal.t[@bits index_width])) : Signal.t =
    let min_index = of_unsigned_int ~width:index_width c.min_index in
    let max_index = of_unsigned_int ~width:index_width c.max_index in
    index >=: min_index &: (index <=: max_index)
  ;;

  let create ~(config : config) scope ({ clock; reset; pe_in } : _ I.t) : _ O.t =
    let spec = Reg_spec.create ~clock ~reset () in
    let open Always in
    (* let%hw[_var] is a shorthand that automatically applies a name to the signal, which
     will show up in waveforms. The [_var] version is used when working with the Always
     DSL. *)
    let%hw_var current_max_val = Variable.reg spec ~width:data_width in
    let%hw_var prev_index = Variable.reg spec ~width:index_width in
    let%hw_var prev_index_valid = Variable.reg spec ~width:1 in
    let output_val = Variable.reg spec ~width:data_width in
    let output_valid = Variable.reg spec ~width:1 in
    let output_index = Variable.reg spec ~width:index_width in
    let output_index_valid = Variable.reg spec ~width:1 in
    let output_flush = Variable.reg spec ~width:1 in
    compile
      [ output_flush <-- pe_in.flush
      ; output_valid <-- pe_in.data.valid
      ; output_val <-- pe_in.data.value
      ; output_index_valid <-- pe_in.index.valid
      ; output_index <-- pe_in.index.value
      ; prev_index <-- pe_in.index.value (* Just a cycle delay for output *)
      ; prev_index_valid <-- pe_in.index.valid (* Just a cycle delay for output *)
      ; if_
          pe_in.flush
          [ current_max_val <-- zero data_width ]
          [ when_
              (pe_in.data.valid &&: pe_in.index.valid)
              [ when_
                  (can_handle_index config pe_in.index.value)
                  [ when_
                      (pe_in.data.value >: current_max_val.value)
                      [ current_max_val <-- pe_in.data.value
                      ; output_val <-- zero data_width
                      ; output_valid <-- gnd
                      ; output_flush <-- vdd
                      ]
                  ]
              ]
          ]
      ];
    let%hw_var output_sum = Variable.reg spec ~width:sum_width in
    let%hw_var output_sum_valid = Variable.reg spec ~width:1 in
    let multiply_const_width = num_bits_to_represent config.multiply_digit_by in
    let multiply_const =
      of_unsigned_int ~width:multiply_const_width config.multiply_digit_by
    in
    compile
      [ if_
          (prev_index_valid.value
           &&: (prev_index.value
                ==: of_unsigned_int ~width:index_width config.last_input_index))
          [ (* TODO validate pe_in.running_sum.valid. Set an error bit if not? *)
            output_sum
            <-- pe_in.running_sum.value
                +: uresize ~width:sum_width (current_max_val.value *: multiply_const)
          ; output_sum_valid <-- vdd
          ]
          [ output_sum_valid <-- gnd ]
      ];
    let input_is_done_reg = Signal.reg spec pe_in.input_is_done in
    { pe_out =
        { data = { value = output_val.value; valid = output_valid.value }
        ; index = { value = output_index.value; valid = output_index_valid.value }
        ; running_sum = { value = output_sum.value; valid = output_sum_valid.value }
        ; flush = output_flush.value
        ; input_is_done = input_is_done_reg
        }
    }
  ;;

  (* The [hierarchical] wrapper is used to maintain module hierarchy in the generated
   waveforms and (optionally) the generated RTL. *)
  let hierarchical ~(config : config) scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"d3_pe" (create ~config)
  ;;
end

(* Input stage for Day3 problem pipeline for a given output size *)
(* Only ascii digits 1-9 or \n are valid inputs. Any other input will set the err output*)
(* Because there isn't really a way to know when the input is done versus there could just be a backup in the UART, we use \n\n to indicate that there is no further input*)
module InputStage = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; reset : 'a
      ; ascii_input : 'a With_valid.t [@bits ascii_bit_width]
      ; input_sum : 'a With_valid.t [@bits sum_width]
      (* Can only be digits 1-9 or newline*)
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { pe_out : 'a PeInterface.t } [@@deriving hardcaml]
  end

  let create scope ({ clock; reset; ascii_input; input_sum } : _ I.t) : _ O.t =
    let spec = Reg_spec.create ~clock ~reset () in
    let index_reset_val = (1 lsl index_width) - 1 in
    let open Always in
    let%hw_var value_buffer = Variable.reg spec ~width:data_width in
    let%hw_var valid_value_buffer = Variable.reg spec ~width:1 in
    let%hw_var index_buffer =
      Variable.reg
        spec
        ~width:index_width
        ~initialize_to:(Bits.of_unsigned_int ~width:index_width index_reset_val)
        ~reset_to:(Bits.of_unsigned_int ~width:index_width index_reset_val)
    in
    let%hw_var valid_index_buffer = Variable.reg spec ~width:1 in
    let%hw_var flush_buffer = Variable.reg spec ~width:1 in
    let%hw_var input_is_done_reg = Variable.reg spec ~width:1 in
    compile
      [ input_is_done_reg <-- gnd
      ; flush_buffer <-- gnd
      ; valid_value_buffer <-- gnd
      ; valid_index_buffer <-- gnd
      ; if_
          ascii_input.valid
          [ (*ASCII is valid. *)
            if_
              (ascii_input.value
               >=: of_unsigned_int ~width:ascii_bit_width (int_of_char '1')
               &&: (ascii_input.value
                    <=: of_unsigned_int ~width:ascii_bit_width (int_of_char '9')))
              [ (*Digit 1-9*)
                (* TODO I wonder what this comparison generates, probably best to just check the few necessary bits? *)
                value_buffer <-- ascii_input.value.:[3, 0]
              ; valid_value_buffer <-- vdd
              ; index_buffer
                <-- index_buffer.value +: of_unsigned_int ~width:index_width 1
              ; valid_index_buffer <-- vdd
              ; flush_buffer <-- gnd
              ]
              [ if_
                  (ascii_input.value
                   ==: of_unsigned_int ~width:ascii_bit_width (int_of_char '\n'))
                  [ (* Newline flushes the input *)
                    valid_value_buffer <-- gnd
                  ; valid_index_buffer <-- gnd
                  ; index_buffer <-- of_unsigned_int ~width:index_width index_reset_val
                  ; if_
                      flush_buffer.value
                      [ (* Second consecutive newline should indicate end *)
                        input_is_done_reg <-- vdd
                      ]
                      [ flush_buffer <-- vdd ]
                  ]
                  [ (* Anything else is an error *) ]
              ]
          ]
          [ valid_value_buffer <-- gnd; valid_index_buffer <-- gnd; flush_buffer <-- gnd ]
      ];
    { pe_out =
        { data = { value = value_buffer.value; valid = valid_value_buffer.value }
        ; index = { value = index_buffer.value; valid = valid_index_buffer.value }
        ; running_sum = input_sum
        ; flush = flush_buffer.value
        ; input_is_done = input_is_done_reg.value
        }
    }
  ;;

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"d3_input_stage" create
  ;;
end

module Pipeline = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; reset : 'a
      ; ascii_input : 'a With_valid.t [@bits ascii_bit_width]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { sum_out : 'a With_valid.t [@bits sum_width] } [@@deriving hardcaml]
  end

  type config =
    { input_length : int (* Length of each input line *)
    ; output_length : int (* Length of number to create from each line *)
    }

  let create ~(config : config) scope ({ clock; reset; ascii_input } : _ I.t) : _ O.t =
    assert (config.input_length > config.output_length);
    let spec = Reg_spec.create ~clock ~reset () in
    let sum_input_wire = Signal.wire sum_width in
    let sum_input_valid = Signal.wire 1 in
    let sum_reg = Signal.reg spec ~enable:sum_input_valid sum_input_wire in
    let input_stage =
      InputStage.hierarchical
        scope
        { clock; reset; ascii_input; input_sum = { value = sum_reg; valid = vdd } }
    in
    let build_pe pe_index input_pe_interface =
      let pe_config =
        PE.
          { min_index = pe_index
          ; max_index = pe_index + (config.input_length - config.output_length)
          ; last_input_index = config.input_length - 1
          ; multiply_digit_by =
              int_of_float (10. ** float_of_int (config.output_length - pe_index - 1))
          }
      in
      PE.hierarchical ~config:pe_config scope { clock; reset; pe_in = input_pe_interface }
    in
    let first_pe = build_pe 0 input_stage.pe_out in
    let rec build_pe_chain pe_index previous_pe =
      if pe_index >= config.output_length
      then previous_pe
      else (
        let next_pe = build_pe pe_index previous_pe in
        build_pe_chain (pe_index + 1) next_pe.pe_out)
    in
    let last_pe = build_pe_chain 1 first_pe.pe_out in
    sum_input_wire <-- last_pe.running_sum.value;
    sum_input_valid <-- last_pe.running_sum.valid;
    let input_is_done_reg = Signal.reg spec last_pe.input_is_done in
    let output_done_flag =
      Signal.reg spec (last_pe.input_is_done &: ~:input_is_done_reg)
    in
    { sum_out = { value = sum_reg; valid = output_done_flag } }
  ;;

  let hierarchical ~(config : config) scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"d3_pipeline" (create ~config)
  ;;
end

(* Doing this for now. If time make a top with a uart that would be able to be loaded on hardware*)
module TopWithoutUart = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; reset : 'a
      ; ascii_input : 'a With_valid.t [@bits ascii_bit_width]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { ascii_output : 'a With_valid.t [@bits ascii_bit_width] }
    [@@deriving hardcaml]
  end

  type config =
    { input_length : int (* Length of each input line *)
    ; p1_output_length : int (* Length of number to create from each line *)
    ; p2_output_length : int (* Length of number to create from each line *)
    }

  let create ~(config : config) scope ({ clock; reset; ascii_input } : _ I.t) : _ O.t =
    let pipeline_p1 =
      Pipeline.hierarchical
        ~config:
          { Pipeline.input_length = config.input_length
          ; output_length = config.p1_output_length
          }
        scope
        { clock; reset; ascii_input }
    in
    let pipeline_p2 =
      Pipeline.hierarchical
        ~config:
          { Pipeline.input_length = config.input_length
          ; output_length = config.p2_output_length
          }
        scope
        { clock; reset; ascii_input }
    in
    let printer =
      Print_decimal_outputs.hierarchical
        scope
        { clock
        ; clear = reset
        ; part1 =
            { value =
                uresize ~width:Print_decimal_outputs.width_bits pipeline_p1.sum_out.value
            ; valid = pipeline_p1.sum_out.valid
            }
        ; part2 =
            { value =
                uresize ~width:Print_decimal_outputs.width_bits pipeline_p2.sum_out.value
            ; valid = pipeline_p2.sum_out.valid
            }
        }
    in
    { ascii_output = printer.byte_out }
  ;;

  let hierarchical ~(config : config) scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"top_without_uart" (create ~config)
  ;;
end
