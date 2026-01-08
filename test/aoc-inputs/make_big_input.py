import random

# Hardcoded values
length = 1000  # number of digits per line
height = 1000  # number of lines

# Fixed seed for deterministic output
random.seed(42)

output_file = "big_input_d3.txt"

with open(output_file, "w") as f:
    for _ in range(height):
        line = "".join(str(random.randint(1, 9)) for _ in range(length))
        f.write(line + "\n")
