using System;
using System.Collections.Generic;
using System.Linq;

public enum Plant
{
    Violets,
    Radishes,
    Clover,
    Grass
}

public class Garden
{
    private Dictionary<string, IEnumerable<Plant>> garden;
    private static readonly string[] DEFAULT_STUDENTS = {
        "Alice", "Bob", "Charlie", "David",
        "Eve", "Fred", "Ginny", "Harriet",
        "Ileana", "Joseph", "Kincaid", "Larry"
    };

    public Garden(IEnumerable<string> children, string windowSills)
    {
        garden = new Dictionary<string, IEnumerable<Plant>>();
        foreach (var pair in children.OrderBy(s => s).Zip(ConvertWindowSills(windowSills), 
            (child, plants) => Tuple.Create(child, plants)))
        {
            string child = pair.Item1;
            IEnumerable<Plant> plants = pair.Item2;
            garden.Add(child, plants);
        }
    }

    public IEnumerable<Plant> GetPlants(string child)
    {
        if (garden.ContainsKey(child))
        {
            return garden[child];
        } else
        {
            return Enumerable.Empty<Plant>();
        }
    }

    public static Garden DefaultGarden(string windowSills)
    {
        return new Garden(DEFAULT_STUDENTS, windowSills);
    }

    private static IEnumerable<IEnumerable<Plant>> ConvertWindowSills(string windowSills)
    {
        string[] rows = windowSills.Split('\n').ToArray();
        if (rows.Length != 2) { throw new ArgumentException("Need a two-line windowsill string"); }
        var row1 = rows[0];
        var row2 = rows[1];
        var chunkedRow1 = row1.Batch(2).Select(cs => String.Concat(cs));
        var chunkedRow2 = row2.Batch(2).Select(cs => String.Concat(cs));
        var interleaved = chunkedRow1.Zip(chunkedRow2, (s1, s2) => s1 + s2);
        return interleaved.Select(s => s.Select(ToPlant));
    }

    private static Plant ToPlant(char c)
    {
        switch(c)
        {
            case 'V': return Plant.Violets;
            case 'R': return Plant.Radishes;
            case 'C': return Plant.Clover;
            case 'G': return Plant.Grass;
            default: throw new ArgumentException("Not a valid plant character");
        }
    }
}

public static class LinqExtensions
{
    public static IEnumerable<IEnumerable<T>> Batch<T>(this IEnumerable<T> source, int batchSize)
    {
        using (var enumerator = source.GetEnumerator())
            while (enumerator.MoveNext())
                yield return YieldBatchElements(enumerator, batchSize - 1);
    }

    private static IEnumerable<T> YieldBatchElements<T>(IEnumerator<T> source, int batchSize)
    {
        yield return source.Current;
        for (int i = 0; i < batchSize && source.MoveNext(); i++)
            yield return source.Current;
    }
}