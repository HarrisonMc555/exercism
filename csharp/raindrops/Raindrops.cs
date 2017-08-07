using System;
using System.Linq;
using System.Collections.Generic;

public static class Raindrops
{
    public static string Convert(int number)
    {
        IEnumerable<string> drops = 
            from dropInfo in dropInfos
            where dropInfo.Item1.Divides(number)
            select dropInfo.Item2;
        if (drops.Any())
        {
            return String.Join("", drops);
        } else {
            return number.ToString();
        }
    }

    private static readonly IEnumerable<Tuple<int, string>> dropInfos = new Tuple<int, string>[] {
            Tuple.Create(3, "Pling"),
            Tuple.Create(5, "Plang"),
            Tuple.Create(7, "Plong")
        };

    private static bool Divides(this int divisor, int num)
    {
        return num % divisor == 0;
    }
}