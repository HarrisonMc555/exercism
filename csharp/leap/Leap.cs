using System;

public static class Leap
{
    public static bool IsLeapYear(int year)
    {
        bool YearDivisibleBy(int divisor) {
            return IsDivisibleBy(year, divisor);
        }
        return (YearDivisibleBy(4) && !YearDivisibleBy(100)) || YearDivisibleBy(400);
    }

    private static bool IsDivisibleBy(int number, int divisor) {
        return number % divisor == 0;
    }
}