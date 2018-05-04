def verify(isbn):
    is_isbn_digit = lambda c: c.isdigit() or c.upper() == 'X'
    isbn = ''.join(d for d in isbn if is_isbn_digit(d))
    # Check length
    if len(isbn) != 10:
        return False
    # Check first 9 digits
    if not all(c.isdigit() for c in isbn[:-1]):
        return False
    # Check last digit
    if not (isbn[-1].isdigit() or isbn[-1].upper() == 'X'):
        return False
    isbn_digit_value = lambda c: int(c) if c.isdigit() else 10
    coefficients = reversed(range(1, 11))
    total = sum(isbn_digit_value(d) * n for d, n in zip(isbn, coefficients))
    return total % 11 == 0
