from functools import reduce

INPUT = '14,58,0,116,179,16,1,104,2,254,167,86,255,55,122,244'
SALT = [17, 31, 73, 47, 23]


def xor(iterable):
    return reduce((lambda x, y: x ^ y), iterable)


def to_hex(number):
    unpadded = hex(number).split('x')[1]
    return unpadded if len(unpadded) == 2 else f'0{unpadded}'


def chunks(iterable, chunk_size=16):
    return (iterable[i:i + chunk_size]
            for i in range(0, len(iterable), chunk_size))


def to_bytes(ascii_string):
    return [ord(c) for c in ascii_string]


def salt(bytes_list):
    return bytes_list + SALT


def identity_mapping(iterable):
    return {index: element for index, element in enumerate(iterable)}


def cycle(iterable, start=0, verbose=False):
    current = start
    while True:
        yield iterable[current]
        current = (current + 1) % len(iterable)


def take(iterable, amount):
    for _ in range(amount):
        yield next(iterable)


def twist(iterable, start_at, length):
    mapping = identity_mapping(iterable)
    indices_to_reverse = list(
        take(cycle(range(len(iterable)), start_at), length))
    reversed_mapping = {
        index: iterable[reversed_index]
        for index, reversed_index in zip(indices_to_reverse,
                                         reversed(indices_to_reverse))}
    mapping.update(reversed_mapping)
    return [mapping[index] for index in range(len(mapping))]


def run_round(circular_list, lengths, position, skip_size):
    for length in lengths:
        circular_list = twist(circular_list, position, length)
        position = (position + length + skip_size) % len(circular_list)
        skip_size += 1
    return circular_list, position, skip_size


def digest(string):
    lengths = salt(to_bytes(string))
    circular_list = range(256)
    position = 0
    skip_size = 0

    for _ in range(64):
        circular_list, position, skip_size = run_round(
            circular_list, lengths, position, skip_size)

    return ''.join([to_hex(xor(chunk)) for chunk in chunks(circular_list)])
