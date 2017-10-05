using System;

public struct Clock
{
    private const int MINUTES_PER_HOUR = 60;
    private const int MAX_HOURS = 24;
    private const int MAX_MINUTES = MINUTES_PER_HOUR * MAX_HOURS;

    private int totalMinutes;

    public Clock(int minutes)
    {
        totalMinutes = RollOver(minutes);
    }

    public Clock(int hours, int minutes)
    {
        totalMinutes = RollOver(hours * MINUTES_PER_HOUR + minutes);
    }

    public int Hours
    {
        get
        {
            return totalMinutes / MINUTES_PER_HOUR;
        }
    }

    public int Minutes
    {
        get
        {
            return totalMinutes % MINUTES_PER_HOUR;
        }
    }

    public int TotalMinutes
    {
        get
        {
            return totalMinutes;
        }
    }

    public Clock Add(int minutesToAdd)
    {
        return new Clock(TotalMinutes + minutesToAdd);
    }

    public Clock Subtract(int minutesToSubtract)
    {
        return new Clock(TotalMinutes + minutesToSubtract);
    }

    private static int RollOver(int minutes)
    {
        return Mod(minutes, MAX_MINUTES);
    }

    private static int Mod(int number, int divisor)
    {
        int x = number % divisor;
        if (x < 0)
        {
            x += divisor;
        }
        return x;
    }

    public override string ToString()
    {
        return $"{Hours:d2}:{Minutes:d2}";
    }
}