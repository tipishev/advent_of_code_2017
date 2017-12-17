def stops_at(length, after, step_size):
    return (after + step_size) % length


def run(step_size, num_iterations):
    buffer = [0]
    current = 0
    for it in range(1, num_iterations + 1):
        stop_at = stops_at(it, current, step_size)
        insert_at = stop_at + 1
        if insert_at == 1:
            buffer.insert(insert_at, it)
        current = insert_at
    print(buffer[1])


run(step_size=355, num_iterations=50_000_000)
