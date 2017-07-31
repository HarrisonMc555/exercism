#include "bob.h"
#include <string>
#include <algorithm>
#include <boost/algorithm/string.hpp>    

std::string trim(const std::string& str, const std::string& whitespace = " ");

std::string bob::hey(const std::string input) {
    std::string trimmed_input = trim(input);
    if (trimmed_input.length() == 0) {
        return "Fine. Be that way!";
    } else if (trimmed_input == boost::algorithm::to_upper_copy(trimmed_input)
               && trimmed_input.length() > 0) {
        return "Whoa, chill out!";
    } else if (trimmed_input.at(trimmed_input.length()-1) == '?') {
        return "Sure";
    } else {
        return "Whatever.";
    }
}

std::string trim(const std::string& str,
                 const std::string& whitespace) {
    const auto strBegin = str.find_first_not_of(whitespace);
    if (strBegin == std::string::npos)
        return ""; // no content

    const auto strEnd = str.find_last_not_of(whitespace);
    const auto strRange = strEnd - strBegin + 1;

    return str.substr(strBegin, strRange);
}
