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
        return f'{self.name} {self.own_weight}: {self.children}'

    @property
    def total_weight(self):
        return self.own_weight + sum([child.total_weight
                                      for child in self.children])

    def __str__(self):
        return f'{self.name} {self.total_weight}'

    def pretty(self, level=0):
        weirdness = '*' if self.is_weird() else ''
        result = '\t' * level + weirdness + str(self)
        result += '\n'
        for child in self.children:
            result += child.pretty(level + 1)
        return result

    def is_weird(self):
        return len(set([c.total_weight for c in self.children])) > 1

    def find_and_describe_deviation(self):
        children = self.children
        weird_children = [c for c in children if c.is_weird()]
        if weird_children:
            assert len(weird_children) == 1
            return weird_children.pop().find_and_describe_deviation()
        counter = MyCounter([c.total_weight for c in children])
        assert len(counter) == 2
        most_common, count = counter.most_common(1).pop()
        least_common = counter.least_common()
        difference = most_common - least_common
        deviant = [c for c in children
                   if c.total_weight == least_common].pop()
        fixed_own_weight = deviant.own_weight + difference
        return (
            f'Normal weight is {most_common} ({count} have it), '
            f'deviant weight is {least_common} for `{deviant.name}`. '
            f'To become normal it needs to change own weight from '
            f'{deviant.own_weight} by {difference} and make it equal to '
            f'{fixed_own_weight}.'
        )


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


records = load('07_input.txt')
#  records = load('07_input_small.txt')

root_name = find_root_name(records)
lookup = {name: (own_weight, children_names)
          for name, own_weight, children_names in records}
tree = Node(root_name, lookup)
print(tree.pretty())
print(tree.find_and_describe_deviation())
