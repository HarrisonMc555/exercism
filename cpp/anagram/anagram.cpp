#include "anagram.h"
#include <string>
#include <vector>
#include <algorithm>
#include <boost/algorithm/string/case_conv.hpp>

using namespace std;

anagram::anagram::anagram(const string word) {
    lower_word = boost::algorithm::to_lower_copy(word);
    letters = get_letters(word);
}

vector<string> anagram::anagram::matches(const vector<string> inputs) {
    vector<string> output;
    for (string s : inputs) {
        if (is_anagram(s)) {
            output.push_back(s);
        }
    }
    return output;
}

bool anagram::anagram::is_anagram(const string input) {
    string lower_input = boost::algorithm::to_lower_copy(input);
    return lower_input != lower_word && get_letters(input) == letters;
}

string anagram::anagram::get_letters(const string input) {
    string output = boost::algorithm::to_lower_copy(output);
    sort(output.begin(), output.end());
    return output;
}
