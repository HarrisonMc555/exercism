def divisible(num, div):
    """Return True if num is divisible by div"""
    return num % div == 0


def is_leap_year(year):
    """Returns True if year is a leap year

    A year is a leap year if it is divisible by 4, unless it is divisible by
    100, unless it is also divisible by 400.
    """
    return divisible(year, 4) and not divisible(year, 100) or divisible(year,
                                                                        400)
