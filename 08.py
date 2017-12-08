from collections import defaultdict


def find_max(dict_):
    return max(dict_.values())


OPS = {
    '>': lambda a, b: a > b,
    '<': lambda a, b: a < b,
    '==': lambda a, b: a == b,
    '!=': lambda a, b: a != b,
    '>=': lambda a, b: a >= b,
    '<=': lambda a, b: a <= b,
}


def act(value, op, arg):
    if op == 'inc':
        return value + arg
    if op == 'dec':
        return value - arg
    raise ValueError


def parse(line):
    tokens = [e.strip() for e in line.split()]
    (value_name, value_op, value_arg,
     if_, condition_name, condition_op, condition_arg) = tokens
    assert value_op in ['dec', 'inc']
    value_arg = int(value_arg)
    assert if_ == 'if'
    assert condition_op in ('>', '<', '==', '!=', '>=', '<=')
    condition_arg = int(condition_arg)
    return (condition_name, condition_op, condition_arg,
            value_name, value_op, value_arg)


def load(filename):
    with open(filename, 'r') as f:
        return [parse(line) for line in f.readlines()]


records = load('08_input.txt')
#  records = load('08_input_small.txt')

registries = defaultdict(int)

global_max = 0
for (condition_name, condition_op, condition_arg,
        value_name, value_op, value_arg) in records:
    if OPS[condition_op](registries[condition_name], condition_arg):
        registries[value_name] = act(registries[value_name],
                                     value_op,
                                     value_arg)
        global_max = max(find_max(registries), global_max)


print(registries)
print(find_max(registries))
print(global_max)
