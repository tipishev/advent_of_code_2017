def factorial(n):
    return 1 if n in (0, 1) else n * factorial(n - 1)


def number_of_permutations(n, k):
    return factorial(n) // factorial(n - k)


def read_phrases():
    phrases = []
    with open('04_input.txt', 'r') as f:
        for line in f.readlines():
            phrases.append(line.strip())
    return phrases


def is_valid(phrase):
    words = phrase.split()
    return len(words) == len(set(words))


def count_valid(phrases):
    return len([phrase for phrase in phrases if is_valid(phrase)])
