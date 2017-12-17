def cycle(length, after):
    position = after
    while True:
        position = position + 1 if position < length - 1 else 0
        yield position


def stops_at(buffer, after, step_size):
    c = cycle(len(buffer), after)
    for _ in range(step_size):
        position = next(c)
        #  value = buffer[position]
        #  print(f'stepping over "{value}"')
    return position


def show(buffer, current):
    print(' '.join([str(e) if idx != current else f'({e})'
                    for idx, e in enumerate(buffer)]))


def run(step_size, num_iterations):
    buffer = [0]
    current = 0
    for it in range(1, num_iterations + 1):
        if it % 10_000 == 0:
            print(it)
        stop_at = stops_at(buffer, current, step_size)
        #  print(f'insert "{it}" after {stop_at}, new current is "{it}"')

        assert stop_at <= len(buffer)
        insert_at = stop_at + 1
        buffer.insert(insert_at, it)  # danger here, circubuffer

        current = insert_at
    print(buffer[insert_at + 1])


#  run(step_size=355, num_iterations=2017)
run(step_size=355, num_iterations=50_000_000)
