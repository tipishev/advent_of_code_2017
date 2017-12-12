def parse(filename):
    result = dict()
    with open(filename, 'r') as f:
        for line in f:
            node, peers = line.strip().replace(' ', '').split('<->')
            key = int(node)
            result[key] = set([int(peer) for peer in peers.split(',')])
    return result


#  data = parse('12_input_small.txt')
data = parse('12_input.txt')

zero_group = {0}
for program, peers in data.items():
    combined_set = {program}.union(peers)
    intersection = zero_group.intersection(combined_set)
    if intersection:
        zero_group = zero_group.union(combined_set)

for _ in range(1000):
    for key in list(zero_group):
        zero_group = zero_group.union(data[key])


print(sorted(zero_group))
print(len(zero_group))
