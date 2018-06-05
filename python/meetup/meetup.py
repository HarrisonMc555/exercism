import datetime


ORDINAL_STRINGS = {
    '1st': 1,
    '2nd': 2,
    '3rd': 3,
    '4th': 4,
    '5th': 5,
}

WEEKDAY_TO_INT = {
    'Monday': 0,
    'Tuesday': 1,
    'Wednesday': 2,
    'Thursday': 3,
    'Friday': 4,
    'Saturday': 5,
    'Sunday': 6,
}

LAST_STRING = 'last'
TEENTH_STRING = 'teenth'


def meetup_day(year, month, day_of_the_week, which):
    if which in ORDINAL_STRINGS:
        return meetup_day_ordinal(year, month, day_of_the_week, which)
    elif which == LAST_STRING:
        return meetup_day_last(year, month, day_of_the_week)
    elif which == TEENTH_STRING:
        return meetup_day_teenth(year, month, day_of_the_week)
    else:
        raise MeetupDayException('Invalid date description')


def meetup_day_ordinal(year, month, day_of_the_week, which):
    first_weekday = first_of_weekday_in_month(year, month, day_of_the_week)
    assert which in ORDINAL_STRINGS
    ordinal = ORDINAL_STRINGS[which]
    n_delta_weeks = ordinal - 1
    date_delta = datetime.timedelta(weeks=n_delta_weeks)
    meetup_date = first_weekday + date_delta
    meetup_month = meetup_date.month
    if month != meetup_month:
        raise MeetupDayException('No {} {} in {}, {}'.format(
            which, day_of_the_week, month, year
        ))
    return meetup_date


def meetup_day_last(year, month, day_of_the_week):
    first_weekday = first_of_weekday_in_month(year, month, day_of_the_week)
    cur_date = first_weekday
    week_delta = datetime.timedelta(weeks=1)
    next_date = cur_date + week_delta
    while next_date.month == month:
        cur_date = next_date
        next_date += week_delta
    assert cur_date.month == month
    return cur_date


def meetup_day_teenth(year, month, day_of_the_week):
    first_weekday = first_of_weekday_in_month(year, month, day_of_the_week)
    meetup_date = first_weekday
    week_delta = datetime.timedelta(weeks=1)
    while not is_teen(meetup_date.day):
        meetup_date += week_delta
    return meetup_date


def first_of_weekday_in_month(year, month, day_of_the_week):
    date = datetime.date(year, month, 1)
    weekday_number = WEEKDAY_TO_INT[day_of_the_week]
    day_delta = datetime.timedelta(days=1)
    while date.weekday() != weekday_number:
        date += day_delta
    return date


def is_teen(n):
    return 13 <= n <= 19


class MeetupDayException(Exception):
    pass
