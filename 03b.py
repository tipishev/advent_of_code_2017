from collections import defaultdict
directions = RIGHT, UP, LEFT, DOWN = '→↑←↓'


def spiral():
    counter = 0
    while True:
        counter += 1
        for _ in range(counter):
            yield RIGHT
        for _ in range(counter):
            yield UP
        counter += 1
        for _ in range(counter):
            yield LEFT
        for _ in range(counter):
            yield DOWN


def adjacent(coordinates):
    x, y = coordinates
    shifts = (-1, 0, 1)
    return ((x + d_x, y + d_y)
            for d_x in shifts
            for d_y in shifts if not (d_x == d_y == 0))


def coordinates():
    coordinates = (0, 0)
    s = spiral()

    while True:
        direction = next(s)
        if direction == RIGHT:
            coordinates = (coordinates[0] + 1, coordinates[1])
        elif direction == UP:
            coordinates = (coordinates[0], coordinates[1] + 1)
        elif direction == LEFT:
            coordinates = (coordinates[0] - 1, coordinates[1])
        elif direction == DOWN:
            coordinates = (coordinates[0], coordinates[1] - 1)
        else:
            raise
        yield coordinates


def values():
    values = defaultdict(int)
    values[(0, 0)] = 1
    c = coordinates()
    while True:
        coords = next(c)
        value = sum(values[a] for a in adjacent(coords))
        values[coords] = value
        yield value


def solve():
    PUZZLE_INPUT = 312051
    v = values()
    current = 0
    while current < PUZZLE_INPUT:
        current = next(v)
    print(current)
