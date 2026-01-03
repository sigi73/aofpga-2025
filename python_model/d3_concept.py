#!/usr/bin/env python3

import logging

from dataclasses import dataclass
from typing import Optional


logger = logging.getLogger(__name__)


@dataclass
class PEInput:
    value: Optional[int]  # To be implemented with valid signal
    index: Optional[
        int
    ]  # index of the sequence this value corresponds to. To be implemented with valid signal. Index can be valid without value, but not vice versa
    flush: bool

    @staticmethod
    def empty():
        return PEInput(value=None, index=None, flush=False)


@dataclass
class PESolution:
    value: Optional[int]


class PE:
    def __init__(self, pe_index: int, input_size: int, output_width: int):
        # Invariants
        assert pe_index >= 0
        assert input_size > 0
        assert output_width > 0

        assert output_width <= input_size
        assert pe_index < output_width

        self.pe_index = pe_index
        self.input_size = input_size
        self.output_width = output_width

        self.current_max_val: Optional[int] = None
        self.next_cycle_input: PEInput = PEInput.empty()
        self.next_output: PEInput = PEInput.empty()

    def set_input(self, input: PEInput):
        logger.debug(f"\t\tPE {self.pe_index} received input: {input}")
        self.next_cycle_input = input

    def compute(self):
        logger.debug(f"\t\tPE {self.pe_index}. Current max: {self.current_max_val}")

        next_output: Optional[PEInput] = None

        if self.next_cycle_input.flush:
            logger.debug("\t\t\tFlush received")
            # On flush, we output nothing and reset our current max
            self.current_max_val = None
        elif self.next_cycle_input.value is not None:
            assert self.next_cycle_input.index is not None
            # If the index is in a range that this PE can handle (helper function)
            current_input = self.next_cycle_input.value
            current_index = self.next_cycle_input.index
            logger.debug(f"\t\t\tGot Input: {current_input}")
            if self._can_handle_index(current_index):
                logger.debug("\t\t\tCan handle index")
                if self.current_max_val is None or current_input > self.current_max_val:
                    logger.debug("\t\t\tUpdating max")
                    self.current_max_val = current_input

                    # We will not output anything next cycle, and downstream should flush
                    next_output = PEInput(value=None, index=current_index, flush=True)

        if next_output is None:
            next_output = self.next_cycle_input

        self.next_output = next_output  # def compute(self):

    #     logger.debug(f"\t\tPE {self.pe_index}. Current max: {self.current_max_val}")
    #
    #     next_output: Optional[PEInput] = None
    #
    #     if self.next_cycle_input.flush:
    #         logger.debug("\t\t\tFlush received")
    #         # On flush, we output nothing and reset our current max
    #         self.current_max_val = None
    #         next_output = self.next_cycle_input
    #     elif self.next_cycle_input.value is not None:
    #         assert self.next_cycle_input.index is not None
    #         # If the index is in a range that this PE can handle (helper function)
    #         current_input = self.next_cycle_input.value
    #         current_index = self.next_cycle_input.index
    #         logger.debug(f"\t\t\tGot Input: {current_input}")
    #         if self._can_handle_index(current_index):
    #             logger.debug("\t\t\tCan handle index")
    #             if self.current_max_val is None or current_input > self.current_max_val:
    #                 logger.debug("\t\t\tUpdating max")
    #                 self.current_max_val = current_input
    #
    #                 # We will not output anything next cycle, and downstream should flush
    #                 next_output = PEInput(value=None, index=current_index, flush=True)
    #             else:
    #                 # Pass the input to the next PE - it is not larger than current max
    #                 next_output = self.next_cycle_input
    #         else:
    #             # Pass the input to the next PE - we cannot operate on it
    #             next_output = self.next_cycle_input
    #     else:
    #         next_output = self.next_cycle_input
    #
    #     self.next_output = next_output

    def get_output(self) -> PEInput:
        return self.next_output

    def _can_handle_index(self, data_index: int) -> bool:
        return pe_can_handle_index(
            pe_index=self.pe_index,
            input_size=self.input_size,
            output_width=self.output_width,
            data_index=data_index,
        )

    def get_pe_solution(self) -> Optional[int]:
        if self.next_cycle_input.index is not None and self.next_cycle_input.index == self.input_size - 1:
            assert self.current_max_val is not None
            return self.current_max_val
        return None


def pe_can_handle_index(pe_index: int, input_size: int, output_width: int, data_index: int) -> bool:
    min_index = pe_index
    max_index = pe_index + (input_size - output_width)
    return min_index <= data_index <= max_index
