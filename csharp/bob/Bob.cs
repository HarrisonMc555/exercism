using System;
using System.Linq;

public static class Bob
{
    public static string Response(string statement)
    {
        string trimmedStatement = statement.Trim();
        if (trimmedStatement == String.Empty) {
            return "Fine. Be that way!";
        } else if (trimmedStatement.Any(char.IsUpper) && !trimmedStatement.Any(char.IsLower)) {
            return "Whoa, chill out!";
        } else if (trimmedStatement.Last() == '?') {
            return "Sure.";
        } else {
            return "Whatever.";
        }
    }
}