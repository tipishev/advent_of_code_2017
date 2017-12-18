from collections import defaultdict

INT, REG = 'int', 'reg'
SND, SET, ADD, MUL, MOD, RCV, JGZ = ('snd', 'set', 'add', 'mul', 'mod',
                                     'rcv', 'jgz')


def lex(tokens):
    def int_or_reg(v):
        try:
            return (INT, int(v))
        except ValueError:
            return (REG, v)
    command, *args = tokens
    return (command, *[int_or_reg(arg) for arg in args])


def tokenize(line):
    return line.rstrip().split()


def read(filename):
    with open(filename, 'r') as f:
        return (line for line in f.readlines())


def parse(filename):
    return [lex(tokenize(line)) for line in read(filename)]


def debug(string):
    print(string)


class Player:

    def __init__(self, filename):
        self.regs = defaultdict(int)
        self.last_played = None
        self.recovered = None
        self.instructions = parse(filename)
        self.position = 0

    def resolve(self, node):
        node_type, node_value = node
        return self.regs[node_value] if node_type == REG else node_value

    def recover(self, value):
        self.recovered = value
        print(f'recovered {value}')
        1/0

    def play(self, value):
        self.last_played = value
        print(f'---playing {value}---')

    def process(self):
        resolve = self.resolve
        regs = self.regs

        instruction = self.instructions[self.position]

        debug(f'pos: {self.position}, {instruction}')

        op, *args = instruction

        if op == JGZ:
            check, offset = args
            if resolve(check) > 0:
                self.position += resolve(offset)
                return

        elif op == SND:
            ((dst_type, dst_value),) = args
            assert dst_type == REG
            self.play(regs[dst_value])

        elif op == SET:
            dst, src = (dst_type, dst_val),  (src_type, src_val) = args
            assert dst_type == REG
            regs[dst_val] = resolve(src)

        elif op == ADD:
            dst, src = (dst_type, dst_val),  (src_type, src_val) = args
            assert dst_type == REG
            regs[dst_val] += resolve(src)

        elif op == MUL:
            dst, src = (dst_type, dst_val),  (src_type, src_val) = args
            assert dst_type == REG
            regs[dst_val] *= resolve(src)

        elif op == MOD:
            dst, src = (dst_type, dst_val),  (src_type, src_val) = args
            assert dst_type == REG
            regs[dst_val] %= resolve(src)

        elif op == RCV:
            check, = args
            if resolve(check) != 0:
                self.recover(self.last_played)
        else:
            raise ValueError(f'`{op}` is an unknown operation')
        self.position += 1
        debug(regs)
        return


#  p = Player('18_input_small.txt')
p = Player('18_input.txt')

while True:
    p.process()
