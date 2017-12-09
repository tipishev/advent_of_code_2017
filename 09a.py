AWAIT, IN_GROUP, GROUP_ENDED, IN_GARBAGE, IGNORE_NEXT = (
    'Await', 'InGroup', 'GroupEnded', 'InGarbage', 'IgnoreNext')


class Machine:
    def __init__(self):
        self.level = 0
        self.score = 0

        def increment_level():
            self.level += 1

        def decrement_level_increase_score():
            self.score += self.level
            self.level -= 1

        self.TRANSITIONS = {
            AWAIT: {
                '{': (IN_GROUP, increment_level),
                '<': IN_GARBAGE,
            },
            IN_GROUP: {
                '{': (IN_GROUP, increment_level),
                '}': (GROUP_ENDED, decrement_level_increase_score),
                '<': IN_GARBAGE,
                ',': IN_GROUP,
            },
            GROUP_ENDED: {
                ',': AWAIT,
                '}': (GROUP_ENDED, decrement_level_increase_score),
            },
            IN_GARBAGE: {
                '!': IGNORE_NEXT,
                '>': IN_GROUP,
                'any': IN_GARBAGE,
            },
            IGNORE_NEXT: {
                'any': IN_GARBAGE,
            },
        }
        self.state = AWAIT

    def __repr__(self):
        return f'{self.state}, level: {self.level}, score: {self.score}'

    @property
    def transitions(self):
        return self.TRANSITIONS[self.state]

    def advance(self, symbol):
        assert len(symbol) == 1
        try:
            advance = self.transitions[symbol]
        except KeyError:
            if 'any' not in self.transitions:
                raise KeyError(f'{symbol} is not valid for state {self.state}')
            advance = self.transitions['any']

        if len(advance) == 2:
            new_state, action = advance
            self.state = new_state
            action()
        else:
            self.state = advance


m = Machine()

with open('09_input.txt', 'r') as stream:
    stream = stream.read().rstrip()
    for symbol in stream:
        m.advance(symbol)

print(m)
