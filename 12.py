INFINITY = 999999


def parse(filename):
    result = dict()
    with open(filename, 'r') as f:
        for line in f:
            node, peers = line.strip().replace(' ', '').split('<->')
            key = int(node)
            result[key] = set([int(peer) for peer in peers.split(',')])
    return result


def Dijkstra(graph, initial):
    distances = dict.fromkeys(graph, INFINITY)

    distances[initial] = 0
    unvisited = set(graph)

    while unvisited:
        items = [(k, v) for (k, v) in distances.items() if k in unvisited]
        current_node, current_node_distance = min(items,
                                                  key=lambda item: item[1])
        unvisited.remove(current_node)

        for neighbor in graph[current_node]:
            current_neighbor_distance = distances[neighbor]
            distances[neighbor] = min(current_neighbor_distance,
                                      current_node_distance + 1)

    return distances


def count_reachable(graph, initial):
    distances = Dijkstra(graph, initial)
    return len([(k, v) for (k, v) in distances.items() if v != INFINITY])


#  graph = parse('12_input_small.txt')
graph = parse('12_input.txt')

print(count_reachable(graph, 0))
