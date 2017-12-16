BILLION = 1_000_000_000


def spin(iterable, at):
    at = int(at)
    return iterable[-at:] + iterable[:-at]


def pos_swap(iterable, pos_1, pos_2):
    it = iterable
    l, r = sorted((int(pos_1), int(pos_2)))
    return it[:l] + it[r] + it[l + 1:r] + it[l] + it[r + 1:]


def name_swap(iterable, name_1, name_2):
    return pos_swap(iterable, iterable.find(name_1), iterable.find(name_2))


def parse(filename):
    commands = {
        's': lambda at: lambda it: spin(it, at),
        'x': lambda p1, p2: lambda it: pos_swap(it, p1, p2),
        'p': lambda n1, n2: lambda it: name_swap(it, n1, n2),
    }
    with open(filename, 'r') as f:
        moves = f.read().rstrip().split(',')
        result = []
        for move in moves:
            command, args = move[0], move[1:].split('/')
            result.append(commands[command](*args))
        return result


STRING = 'abcdefghijklmnop'
ops = parse('16_input.txt')
string = 'abcdefghijklmnop'

#  ops = parse('16_input_small.txt')
#  string = 'abcde'


def permute(s):
    new_string = s
    for op in ops:
        new_string = op(new_string)
    return new_string


#  def create_permutator(before, after):
#      permutation = [(index_before, after.find(letter))
#                     for index_before, letter in enumerate(before)]
#      print(permutation)
#      return lambda it: [it[after] for before, after in permutation]


#  p = create_permutator('abcde', 'baedc')

def brute_count():
    string = 'abcdefghijklmnop'
    counter = 0
    while True:
        if counter % 100 == 0:
            print(counter)
        string = permute(string)
        counter += 1
        if string == 'abcdefghijklmnop':
            break
    return counter


def run():
    repeats_after = brute_count()
    string = 'abcdefghijklmnop'
    for _ in range(BILLION % repeats_after):
        string = permute(string)
    print(string)


run()
