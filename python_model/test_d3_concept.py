#!/usr/bin/env python3

import logging
import pytest

from typing import Optional
from d3_concept import PE, PEInput

logger = logging.getLogger(__name__)


def instantiate_pes(input_size: int, output_width: int) -> list[PE]:
    pes = []
    for pe_index in range(output_width):
        pe = PE(pe_index=pe_index, input_size=input_size, output_width=output_width)
        pes.append(pe)
    return pes


def run(input: str, output_width: int) -> list[int]:
    input_values = [int(x) for x in input]
    input_size = len(input_values)

    pes = instantiate_pes(input_size=input_size, output_width=output_width)

    sols: list[Optional[int]] = [None] * output_width

    def step(input: Optional[int], index: int, flush: bool):
        base_input = PEInput(value=input, index=index, flush=flush)
        if flush:
            logger.debug("Flushing pipeline...")
        else:
            logger.debug(f"Feeding value: {input}")

        logger.debug("\tSetting inputs for PEs...")
        pes[0].set_input(base_input)
        next_input = pes[0].get_output()
        for pe in pes[1:]:
            pe.set_input(next_input)
            next_input = pe.get_output()

        logger.debug("\tComputing PEs...")
        for pe in pes:
            pe.compute()
            sol = pe.get_pe_solution()
            if sol is not None:
                logger.debug(f"\t\t\tGot solution during step: {sol}")
                assert sols[pe.pe_index] is None
                sols[pe.pe_index] = sol

    for index, val in enumerate(input_values):
        step(input=val, index=index, flush=False)
    for _ in range(output_width - 1):
        step(input=None, index=-1, flush=True)

    # Collect outputs from all PEs
    output_sols: list[int] = []
    for s in sols:
        # Pyright typechecker funness
        assert s is not None
        output_sols.append(s)
    return output_sols


@pytest.mark.parametrize(
    "input, expected_output",
    [
        ("9876", [9, 8]),
        ("8119", [8, 9]),
        ("987654321111111", [9, 8]),
        ("811111111111119", [8, 9]),
        ("234234234234278", [7, 8]),
        ("818181911112111", [9, 2]),
        ("987654321111111", [9, 8, 7, 6, 5, 4, 3, 2, 1, 1, 1, 1]),
        ("811111111111119", [8, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 9]),
        ("234234234234278", [4, 3, 4, 2, 3, 4, 2, 3, 4, 2, 7, 8]),
        ("818181911112111", [8, 8, 8, 9, 1, 1, 1, 1, 2, 1, 1, 1]),
    ],
)
def test_basic(input: str, expected_output: list[int]):
    ret = run(input, output_width=len(expected_output))
    assert ret == expected_output, f"Expected {expected_output}, got {ret}"


def run_pipelined(input: list[str], output_width: int) -> list[list[int]]:
    assert len(input) > 0
    input_size = len(input[0])
    for input_str in input:
        assert len(input_str) == input_size

    pes = instantiate_pes(input_size=input_size, output_width=output_width)

    current_sol: list[Optional[int]] = [None] * output_width
    all_sols: list[list[int]] = []

    def step(input: Optional[int], index: int, flush: bool):
        base_input = PEInput(value=input, index=index, flush=flush)
        if flush:
            logger.debug("Flushing pipeline...")
        else:
            logger.debug(f"Feeding value: {input}")

        logger.debug("\tSetting inputs for PEs...")
        pes[0].set_input(base_input)
        next_input = pes[0].get_output()
        for pe in pes[1:]:
            pe.set_input(next_input)
            next_input = pe.get_output()

        logger.debug("\tComputing PEs...")
        for pe_index, pe in enumerate(pes):
            pe.compute()
            sol = pe.get_pe_solution()
            if sol is not None:
                logger.debug(f"\t\t\tGot solution: {sol}")
                assert current_sol[pe.pe_index] is None
                current_sol[pe.pe_index] = sol

                if pe_index == output_width - 1:
                    # Last PE, we have a full solution
                    sol_snapshot: list[int] = []
                    for s in current_sol:
                        # Pyright typechecker funness
                        assert s is not None
                        sol_snapshot.append(s)
                    all_sols.append(sol_snapshot)

                    # Reset current solution for next
                    for i in range(len(current_sol)):
                        current_sol[i] = None

    for input_str in input:
        input_values = [int(x) for x in input_str]
        for index, val in enumerate(input_values):
            step(input=val, index=index, flush=False)
        # Flush the pipeline for one step (the newline input)
        for _ in range(1):
            step(input=None, index=-1, flush=True)
    # Flush the pipeline for remaining steps
    for _ in range(output_width - 1):
        step(input=None, index=-1, flush=True)

    return all_sols


@pytest.mark.parametrize(
    "inputs, expected_output",
    [
        (["9876"], [[9, 8]]),
        (["8119"], [[8, 9]]),
        (["9876", "8119"], [[9, 8], [8, 9]]),
        (
            [
                "987654321111111",
                "811111111111119",
                "234234234234278",
                "818181911112111",
            ],
            [
                [9, 8, 7, 6, 5, 4, 3, 2, 1, 1, 1, 1],
                [8, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 9],
                [4, 3, 4, 2, 3, 4, 2, 3, 4, 2, 7, 8],
                [8, 8, 8, 9, 1, 1, 1, 1, 2, 1, 1, 1],
            ],
        ),
    ],
)
def test_pipelined(inputs: list[str], expected_output: list[list[int]]):
    output_width = len(expected_output[0])
    for output in expected_output:
        assert len(output) == output_width

    ret = run_pipelined(inputs, output_width=output_width)
    assert ret == expected_output, f"Expected {expected_output}, got {ret}"
