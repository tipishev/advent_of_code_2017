
from knothash import digest

#  INPUT = 'amgozmfv'
#  FILE_NAME = '14_precomputed.txt'

EXAMPLE = 'flqrgnkx'
FILE_NAME = '14_precomputed_example.txt'


def to_bin_string(hex_char):
    hex_num = int(hex_char, 16)
    return f'{hex_num:04b}'


def to_bit_strings(seed):
    result = []
    digests = [digest(f'{seed}-{num}') for num in range(128)]
    for d in digests:
        binary_string = ''.join([to_bin_string(c) for c in d])
        result.append(binary_string)
    return result


def load():
    with open(FILE_NAME, 'r') as f:
        return f.readlines()


def save(bit_strings):
    with open(FILE_NAME, 'w') as f:
        f.write('\n'.join(bit_strings))


def count_ones(bit_strings):
    counter = 0
    for string in bit_strings:
        for char in string:
            if char == '1':
                counter += 1
    return counter


bit_strings = to_bit_strings(EXAMPLE)
save(bit_strings)

bit_strings = load()
print(count_ones(bit_strings))
