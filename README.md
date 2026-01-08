"Advent of FPGA 2025"
===========================
Based off the [Hardcaml Template Project](https://github.com/janestreet/hardcaml_template_project/tree/with-extensions)
## Overview
This is my first OCaml (and Hardcaml) project! I dove right in and don't really understand the package management right now/how to create a reproducible environment. The environment is setup following the instructions in the template repo, and therefore is using Ocaml 5.2.0+ox. The hardcaml packages are all on version v0.18~preview.130.76+222.

It has been a while since I worked with hardware design, this was a very fun way to get back into it!

### Run tests
Run the tests with `dune test`. The test harness expects the Advent of Code input files at tests/aoc-inputs/*.txt (`d3.txt` for day 3 input). This is not included in the repo due to the guidelines from Advent of Code.

### Generating RTL

To generate RTL, run the compiled `generate.exe` binary, which will print the Verilog source:
```
bin/generate.exe d3
```

## Design - D3
I have only implemented day 3.

Day 3 was chosen as it is a problem that appears to be well suited to be handled in hardware, that I believe I have a design for that can scale very well (with an unfortunate requirement of knowing the length of the input line).I don't want to just replicate my software solution in hardware, but implement a solution that can have no stalls and high throughput.

The design has "realistic IO" in that the top level module input of a 8 bit wide ascii input with a valid, and has an 8 bit wide ascii output with valid. This would be hooked up to a UART module, but I haven't implemented that yet.

### Problem summary
You are given multiple lists of numbers of the same length, each list containing only the digits 1-9. In part 1, you are asked to select 2 numbers from the list, in order, such that when the numbers are concatenated you get the largest possible number from the list. You must then sum up the answers for each list of numbers. Part 2 is similar, except 12 numbers have to be selected.

The input provided has 200 rows, with 100 digits in each list.

### Methodology summary
Each line can (and should) be treated independently. Within a line, the most significant output digits should be the largest digits possible. In addition, for each output digit, there are a fixed set of digits in the input where that output digit can come from. For example, in this small example where there are 5 inputs, and we want 3 output digits.

```
Input:
abcde

Output:
xyz
```

The output `x` must be contained within the inputs `abc`, as it must leave at least 2 spaces for the remaining digits. The output `y` must be contained within the inputs `bcd`, and the output `z` must come from the inputs `cde`.

Additionally, we know that the output must stay ordered, so each output digit must come after the previous. With this information, we realize that each digit can have fairly local reasoning that we can replicate in a chain. The logic is as follows: As long as it knows the place in the input stream of each digit, then it can select it's own maximum within the allowed indices. If the previous digit finds a maximum, then the current digit must find a new maximum. However, this does require that the number of digits in the input list is known.

The design as follows hopefully allows for high throughput by minimizing any stalls and pipelining the input through each PE (processing element) that corresponds to each output digit in a systolic architecture. I say hopefully because unfortunately I currently do not have a Xilinx license and have not had a chance to experiment with any open source toolchains.

### Design
#### Processing element (PE)
A processing element is instantiated for each output digit, and is connected to the next and previous elements in the chain. Each clock cycle, it shifts in:
- The current input digit
- The index of that digit in the chain
- A running sum
- A flush command

If this input index is in the valid range for this PEs output digit, and if this value is greater than its currently stored digit, then save the digit and send a flush command to the next PE.

If a flush command is received, clear our stored digit and forward the flush command to the next PE.

If this index corresponds to the end of the stream (based on the index), shift our digit up to the correct place (i.e. multiply by 10 for the second to last digit) and add it to the running sum which we shift out.

With this design, the input can be pipelined through each PE for each digit, and the input and output can be as long as we want! For longer inputs we simply keep processing the stream (and each PE will be able to store a wider range of digits). For longer outputs we would add more PEs, but that should only be limited by hardware resources, not a limit on clock rate since each step is pipelined.

A simple model of this exists in the `python_model/` directory, but does not include the running sum portion.


#### Input Stage
We need to massage the input ascii stream into a format that the PEs can use. The input stage does that, by running a counter for each digit, and converting the ascii input into a decimal input to be passed to the PE chain. In addition, on newline it sends a flush down the stream so that each PE is reset and ready for the new stream.

One deviation from the Advent of Code problem is that the input stage knows that the input is done by a double newline ("\n\n") input. This is not part of the original problem input, but software reading from the file will get an EOF input, which I represent here with "\n\n". A possible alternative implementation could look at the delay between inputs before deciding that the input is done and to print the solution on the output. We feed this is_done through the PE chain so that we can output the final sum when complete.


#### Overall pipeline
By connecting the input stage to the chain of PEs, we can feed the input ascii stream into the output PEs. The running sum output of the last PE will then contain the sum for that line. By feeding that running sum back into the first PE, we will then start summing the next line into our existing running sum. When the `is_done` signal is outputted by the final PE, we can print the result.

The Print_decimal_outputs module from the [advent-of-hardcaml-2024](https://github.com/asinghani/advent-of-hardcaml-2024/tree/main) repo was used for the output. I also intended to connect the output of this module, and the input of the input stage to the UART module from this repo, but haven't had time to write the tests for it.

### Tests
The tests in the `test/` directory test the different modules and the full pipeline. The full pipeline is also tested with a very large input (validated on my original AoC solution, hope that is right...).

### Limitations + improvements
#### Input size must be known
I think the main limitation here is that you must know the size of the input. I haven't fleshed it out fully... I think if each PE maintained a stack with the max value seen, and the index of that value, then it could determine which value to used at the end of the stream. However, I'm not yet sure if this could be implemented without needing to stall the pipeline, but it is something I would like to experiment with.

#### Synthesizeable?
There is nothing complicated here, and no big fan in/out chains. Only addition, and multiplication by constants (powers of 10). The output module uses division by a constant, but I think double-dabble could also be used here (with some extra logic to detect and skip leading zeros).

I intended to experiment with some open source toolchains but haven't had time. Below is a discussion of how the hardware changes with the input sizes.

- The length of each input line doesn't change much, just some combinational logic to determine whether a PE can use that input or not.
- The length of the output changes the number of PEs, and potentially the width of the running sum bus. The number of PEs should scale very well at any given clock rate, as each is pipelined to the next. For the running sum, I don't have a good intuition on how fast you can do large bit width adds. This implementation is hardcoded to 60 bits, as that is what the `print_decimal_outputs` module is set to. 48 bits are needed for the maximum possible size in the advent of code input, but as we make the output much wider, or add many more input rows, then more sum bits are needed.
- The number of output rows also could affect the required width of the running sum bus.

Definitely would like to add a uart, synthesize it, and test it on hardware!

#### Improve tests
These tests are not very comprehensive, half of my validation was probably just looking at waveforms. I think updating to use `hardcaml_test_harness` would make writing the tests a little easier.
