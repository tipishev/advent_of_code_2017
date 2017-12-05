def run(offsets):
    acceptable_range = range(len(offsets))

    index = 0
    jump_count = 0

    while index in acceptable_range:
        saved_current_value = offsets[index]
        if saved_current_value >= 3:
            offsets[index] -= 1
        else:
            offsets[index] += 1
        jump_count += 1
        index += saved_current_value
        #  print(f'new index: {index}')
        #  print(offsets)

    print(f'final state: {offsets}')
    return jump_count


with open('05_input.txt', 'r') as f:
    offsets = [int(n) for n in f.read().split('\n') if n != '']

#  offsets = [0, 3, 0, 1, -3]

print(run(offsets))
