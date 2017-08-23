using System;

public static class TwoFer
{
    public static string Name(string input = null)
    {
        string name = input ?? "you";
        return "One for " + name + ", one for me.";
    }
}
