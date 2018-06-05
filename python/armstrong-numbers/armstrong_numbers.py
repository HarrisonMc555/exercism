def is_armstrong(number):
    digits = to_digits(number)
    num_digits = len(digits)
    return number == sum(digit ** num_digits for digit in digits)


def to_digits(number):
    return [int(d) for d in str(number)]
