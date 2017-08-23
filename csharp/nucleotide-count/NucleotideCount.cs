using System;
using System.Collections.Generic;

public class DNA
{
    public IDictionary<char, int> NucleotideCounts { get; private set; }
    private static readonly IDictionary<char, int> EmptyNucleotideCounts =
        new Dictionary<char, int>
        {
            { 'A', 0 },
            { 'G', 0 },
            { 'C', 0 },
            { 'T', 0 }
        };

    public DNA(string sequence)
    {
        NucleotideCounts = CountNucleotides(sequence);
    }

    public int Count(char nucleotide)
    {
        return GetCount(NucleotideCounts, nucleotide);
    }

    private IDictionary<char, int> CountNucleotides(string sequence)
    {
        var d = new Dictionary<char, int> (EmptyNucleotideCounts);
        foreach (char c in sequence)
        {
            d[c] = GetCount(d, c) + 1;
        }
        return d;
    }

    private static bool IsValidNucleotide(char c)
    {
        return EmptyNucleotideCounts.ContainsKey(c);
    }

    private static int GetCount(IDictionary<char, int> d, char c)
    {
        if (IsValidNucleotide(c))
        {
            return d[c];
        }
        else
        {
            throw new InvalidNucleotideException();
        }
    }
}

public class InvalidNucleotideException : Exception { }
