MAX = 700  # the input is less than 700^2 = 490_000

ODD_NUMBERS = range(1, MAX, 2)
THRESHOLDS = [lev * lev for lev in ODD_NUMBERS]


def flatten(l):
    return [e for sublist in l for e in sublist]


def middle(n):
    assert n % 2 == 1
    return n // 2 + 1


def distance(x, y):
    return abs(x) + abs(y)


def to_level(location):
    for (level, threshold) in enumerate(THRESHOLDS):
        if threshold >= location:
            return level


def to_components(level):
    return range(THRESHOLDS[level - 1] + 1, THRESHOLDS[level] + 1)


def bisect(l):
    length = len(l)
    assert length % 2 == 0
    halfway = length // 2
    return [l[:halfway], l[halfway:]]


def quadsect(l):
    return [list(sl) for sl in flatten([bisect(half) for half in bisect(l)])]


def to_equigroups(k):
    groups = list(zip(*quadsect(to_components(k))))
    corners = groups.pop()
    result = {corners: 2 * k}

    middle_idx = middle(len(groups))
    for idx, group in enumerate(groups, 1):
        if idx <= middle_idx:
            result[group] = 2 * k - idx
        elif idx > middle_idx:
            result[group] = idx
    return result


def to_distance(location):
    level = to_level(location)
    equigroups = to_equigroups(level)
    for group, distance in equigroups.items():
        if location in group:
            return distance


assert to_level(1) == 0
assert to_level(2) == 1
assert to_level(8) == 1
assert to_level(9) == 1
assert to_level(24) == 2
assert to_level(49) == 3
assert to_level(50) == 4

assert to_components(1) == range(2, 10)
assert to_components(2) == range(10, 26)

assert bisect([1, 2, 3, 4]) == [[1, 2], [3, 4]]
