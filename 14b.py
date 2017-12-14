
class Grid:

    def __init__(self, filename):
        self._grid = self._load(filename)
        self._size = size = len(self._grid)
        self._all_coordinates = [(x, y)
                                 for x in range(size)
                                 for y in range(size)]
        assert size == 128

    def _load(self, filename):
        with open(filename, 'r') as f:
            result = []
            for line in f.readlines():
                result.append([int(c) for c in line.rstrip()])
            return result

    def neighbors(self, x, y):
        size = self._size
        return [
            (n_x, n_y) for (n_x, n_y) in [(x - 1, y), (x + 1, y),
                                          (x, y - 1), (x, y + 1)]
            if n_x in range(size) and n_y in range(size)
        ]

    def colorize(self, x, y, color):
        grid = self._grid

        assert grid[x][y] == 1
        assert color > 1

        grid[x][y] = color

        for n_x, n_y in self.neighbors(x, y):
            if grid[n_x][n_y] == 1:
                self.colorize(n_x, n_y, color)

    def count_regions(self):
        grid = self._grid
        counter = 0

        for x, y in self._all_coordinates:
            if grid[x][y] == 1:
                self.colorize(x, y, counter + 2)
                counter += 1
        return counter


#  grid = Grid('14_precomputed_example.txt')
grid = Grid('14_precomputed.txt')
print(grid.count_regions())
