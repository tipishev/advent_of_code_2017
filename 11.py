offset = {
    'n': (0, +1),
    'ne': (+1, +1),
    'se': (+1, 0),
    's': (0, -1),
    'sw': (-1, -1),
    'nw': (-1, 0),
}

#  path = 'se,sw,se,sw,sw'.split(',')
with open('11_input.txt', 'r') as f:
    path = f.read().rstrip().split(',')

position = (0, 0)
dist = 0
furthest = 0

for direction in path:
    x, y = position
    dx, dy = offset[direction]
    x, y = position = (x + dx, y + dy)
    dist = max(abs(x), abs(y))
    furthest = max(dist, furthest)

print(f'pos: {position}, dist: {dist}, furthest: {furthest}')
