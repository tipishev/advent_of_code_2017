from collections import Counter


class MyCounter(Counter):
    def least_common(self):
        return self.most_common()[:-1 - 1:-1][0][0]


def flatten(list_):
    return [element for sublist in list_ for element in sublist]


class Node:
    def __init__(self, own_name, lookup):
        self.name = own_name
        self.own_weight, children_names = lookup.pop(own_name)
        self.children = [Node(name, lookup) for name in children_names]

    def __repr__(self):
        return f'({self.name}:{self.children})'


def parse(line):
    tokens = [e.strip(',()') for e in line.split()]
    own_name, own_weight, *children = tokens
    children_names = children[1:] if children else []
    own_weight = int(own_weight)
    return (own_name, own_weight, children_names)


def load(filename):
    with open(filename, 'r') as f:
        return [parse(line) for line in f.readlines()]


def find_root_name(records):
    all_names = flatten([[own_name] + children_names
                         for own_name, _, children_names in records])
    return MyCounter(all_names).least_common()


#  records = load('07_input.txt')
records = load('07_input_small.txt')

root_name = find_root_name(records)
lookup = {name: (own_weight, children_names)
          for name, own_weight, children_names in records}
tree = Node(root_name, lookup)
