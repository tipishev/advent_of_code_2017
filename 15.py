def to_bin_string(num):
    return format(num, '032b')


def generator(factor, previous, divisible_by=1):
    while True:
        new = (previous * factor) % 2147483647
        if new % divisible_by == 0:
            yield new
        previous = new

#  NUMBER_OF_ITERATIONS = 40_000_000
NUMBER_OF_ITERATIONS = 5_000_000

#  A = generator(16807, 65, 4)  # from example
#  B = generator(48271, 8921, 8)
A = generator(16807, 679, 4)  # actual input
B = generator(48271, 771, 8)


def run():
    counter = 0
    for (a, b), i in zip(zip(A, B), range(NUMBER_OF_ITERATIONS)):
        if i % 1_000_000 == 0:
            print(i, counter)
        if ((a ^ b) % (2 ** 16) == 0):
            counter += 1
    print(counter)


run()
