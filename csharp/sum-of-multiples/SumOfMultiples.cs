using System;
using System.Collections.Generic;
using System.Linq;
using System.Diagnostics;

public static class SumOfMultiples
{
    public static int To(IEnumerable<int> multiples, int max)
    {
        IEnumerable<int> concatMultiples(IEnumerable<int> ms, int x)
        {
            return ms.Concat(getMultiples(x, max));
        }
        return multiples.Aggregate(Enumerable.Empty<int>(), concatMultiples)
            .Distinct().Sum();
    }


    private static IEnumerable<int> getMultiples(int x, int max)
    {
        return x.Multiples().TakeWhile(n => n < max);
    }

    private static IEnumerable<int> Multiples(this int x)
    {
        int cur = x;
        while (true) {
/*             System.Console.Write("\t");
            System.Console.Write(x);
            System.Console.Write(" cur: ");
            System.Console.WriteLine(cur); */
            yield return cur;
            cur += x;
        }
    }
}