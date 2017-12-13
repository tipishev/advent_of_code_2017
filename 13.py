def read_input(filename):
    result = dict()
    with open(filename, 'r') as f:
        for line in f.readlines():
            layer, range_ = [int(num) for num in line.strip(' ').split(':')]
            result[layer] = range_
    return result


#  depth_to_range = read_input('13_input_small.txt')
depth_to_range = read_input('13_input.txt')


def part_1():

    def scanner_pos(range_, time):
        m = range_ - 1
        t_ = time % (2 * m)
        return - abs(t_ - m) + m

    severity = 0
    for time in range(max(depth_to_range) + 1):
        depth = time
        if depth in depth_to_range:
            range_ = depth_to_range[depth]
            scan_pos = scanner_pos(range_, time)
            if scan_pos == 0:
                severity += depth * range_
    print(severity)


def part_2():
    layers = sorted(depth_to_range.items())

    def scanner_caught(range_, time):
        m = range_ - 1
        t_ = time % (2 * m)
        return m == abs(t_ - m)

    def get_catcher(delay):
        for depth, range_ in layers:
            if scanner_caught(range_, depth + delay):
                return depth

    delay = 0
    while True:
        if delay % 1000 == 0:
            print(delay)
        catcher = get_catcher(delay)
        if catcher is None:
            print(f'undetected with {delay}')
            break
        delay += 1


part_2()
