using System;
using System.Collections.Generic;
using System.Linq;

public class Allergies
{
    private static readonly Dictionary<string, int> AllergyDict = new Dictionary<String, int>()
    {
        { "eggs",         1 },
        { "peanuts",      2 },
        { "shellfish",    4 },
        { "strawberries", 8 },
        { "tomatoes",     16 },
        { "chocolate",    32 },
        { "pollen",       64 },
        { "cats",         128 },
    };

    private IEnumerable<string> allergies;
    private int mask;

    public Allergies(int mask)
    {
        this.mask = mask;
        allergies = from kvp in AllergyDict
                    where IsAllergicTo(kvp.Key, mask)
                    select kvp.Key;
    }

    public bool IsAllergicTo(string allergy)
    {
        return IsAllergicTo(allergy, mask);
    }

    public IList<string> List()
    {
        return allergies.ToList();
    }

    private static bool IsAllergicTo(string allergy, int mask)
    {
        int allergyScore = AllergyDict[allergy];
        int scoreTest = mask & allergyScore;
        return scoreTest != 0;
    }
}