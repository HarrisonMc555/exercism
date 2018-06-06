def square_of_sum(count):
    n = sum(range(count + 1))
    return n * n


def sum_of_squares(count):
    return sum(n * n for n in range(count + 1))


def difference(count):
    return abs(sum_of_squares(count) - square_of_sum(count))
