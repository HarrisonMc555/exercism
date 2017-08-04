using System;
// using System.Linq;

public static class Squares
{
    public static int SquareOfSums(int max)
    {
         // return Enumerable.Range(1, max).Sum().Square();
         return (max*(max+1)/2).Square();
    }

    public static int SumOfSquares(int max)
    {
        // return Enumerable.Range(1, max).Select(Square).Sum();
        return max*(max + 1)*(2*max + 1)/6;
    }

    public static int DifferenceOfSquares(int max)
    {
        return SquareOfSums(max) - SumOfSquares(max);
    }

    private static int Square(this int x) {
        return x * x;
    }
}