using System;

public static class Gigasecond
{
    public static DateTime Date(DateTime birthDate)
    {
        return birthDate.AddSeconds(giga);
    }

    private const int giga = 1000000000;
}