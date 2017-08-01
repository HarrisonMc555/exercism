#include <string>
#include <vector>

namespace anagram {
    class anagram {
    public:
        anagram(const std::string word);
        std::vector<std::string> matches(const std::vector<std::string> inputs);
    private:
        std::string lower_word;
        std::string letters;
        bool is_anagram(const std::string input);
        std::string get_letters(std::string input);
    };
}
