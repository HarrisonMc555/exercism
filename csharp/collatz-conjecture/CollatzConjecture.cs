using System;

public static class CollatzConjecture
{
    private const int FINAL_COLLATZ_NUMBER = 1;

    public static int Steps(int number)
    {
        int steps = 0;
        while (number != FINAL_COLLATZ_NUMBER)
        {
            number = NextCollatzNumber(number);
            steps++;
        }
        return steps;
    }

    private static int NextCollatzNumber(int n)
    {
        if (n <= 0)
        {
            throw new ArgumentException("Only positive integers in Collatz sequence");
        } else if (n == FINAL_COLLATZ_NUMBER)
        {
            throw new ArgumentException(FINAL_COLLATZ_NUMBER + " is ending number for Collatz sequence.");
        } else if (n.IsEven())
        {
            return n / 2;
        } else
        {
            return 3 * n + 1;
        }
    }

    public static bool IsEven(this int x)
    {
        return x % 2 == 0;
    }
}