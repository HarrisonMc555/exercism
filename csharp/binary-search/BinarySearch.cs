using System;

public class BinarySearch
{
    private const int NOT_FOUND_INDEX = -1;

    private int[] values;

    public BinarySearch(int[] input)
    {
        values = (int[])input.Clone();
        Array.Sort(values);
    }

    public int Find(int value)
    {
        return FindRecursive(value, 0, values.Length);
    }

    private int FindRecursive(int value, int minBound, int maxBound)
    {
        if (minBound >= maxBound)
        {
            return NOT_FOUND_INDEX;
        }
        int index = (minBound + maxBound) / 2;
        int result = values[index];
        if (result < value)
        {
            return FindRecursive(value, index + 1, maxBound);
        }
        else if (result > value)
        {
            return FindRecursive(value, minBound, index);
        }
        else
        {
            return index;
        }
    }
}
