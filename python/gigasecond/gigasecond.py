import datetime


def add_gigasecond(birth_date):
    giga = 1000000000
    delta = datetime.timedelta(seconds=giga)
    return birth_date + delta
