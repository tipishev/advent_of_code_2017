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


def run():

    # circular_list = range(5)
    #  lengths = [3, 4, 1, 5]

    circular_list = range(256)
    lengths = [14, 58, 0, 116, 179, 16, 1, 104,
               2, 254, 167, 86, 255, 55, 122, 244]

    position = 0
    for skip_size, length in enumerate(lengths):
        circular_list = twist(circular_list, position, length)
        position = (position + length + skip_size) % len(circular_list)

    return circular_list[0] * circular_list[1]


print(run())
