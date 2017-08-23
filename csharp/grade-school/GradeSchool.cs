using System;
using System.Collections.Generic;
using System.Linq;

public class School
{
    private Dictionary<string, int> students;

    public School()
    {
        students = new Dictionary<string, int>();
    }

    public void Add(string student, int grade)
    {
        students.Add(student, grade);
    }

    public IEnumerable<string> Roster()
    {
        return students
            .OrderBy(Grade)
            .ThenBy(Name)
            .Select(Name);
    }

    public IEnumerable<string> Grade(int grade)
    {
        return students
            .Where(pair => Grade(pair) == grade)
            .OrderBy(Name)
            .Select(Name);
    }

    private string Name(KeyValuePair<string, int> pair)
    {
        return pair.Key;
    }

    private int Grade(KeyValuePair<string, int> pair)
    {
        return pair.Value;
    }
}
