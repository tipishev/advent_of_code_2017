def cycle(start, length):
    assert start in range(length)
    current = start
    yield current
    while True:
        current += 1
        if current not in range(length):
            current = 0
        yield current


class Debugger():

    def __init__(self, blocks):
        self.blocks = blocks
        self.seen = set()

    def found_loop(self):
        snapshot = tuple(self.blocks)
        print(snapshot)
        if snapshot in self.seen:
            print(f'saw {snapshot} before')
            return True
        else:
            self.seen.add(snapshot)
            return False

    @property
    def biggest_block_index(self):
        winner_index, winner_value = None, None
        for index, value in enumerate(self.blocks):
            if winner_value is None or value > winner_value:
                winner_index, winner_value = index, value
        return winner_index

    def distribute(self, donor_index):
        blocks = self.blocks
        booty = blocks[donor_index]
        blocks[donor_index] = 0
        start = 0 if donor_index == len(blocks) - 1 else donor_index + 1
        recipient_indices = cycle(start, len(blocks))
        while booty > 0:
            recipient_index = next(recipient_indices)
            blocks[recipient_index] += 1
            booty -= 1

    def run(self):
        counter = 0
        while not self.found_loop():
            donor_index = self.biggest_block_index
            self.distribute(donor_index)
            counter += 1
        return counter


#  d = Debugger([2, 8, 8, 5, 4, 2, 3, 1, 5, 5, 1, 2, 15, 13, 5, 14])
d = Debugger([0, 13, 12, 10, 9, 8, 7, 5, 3, 2, 1, 1, 1, 10, 6, 5])
#  d = Debugger([0, 2, 7, 0])
