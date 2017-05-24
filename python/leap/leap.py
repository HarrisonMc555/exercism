def is_leap_year(year):
    if year % 25:
        return year % 4 == 0
    else:
        return year % 16 == 0
