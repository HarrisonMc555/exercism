class Anagram(source: String) {
    val sourceLetters: Map<Char, Int>
    val source: String

    init {
        this.source = source.toLowerCase()
        sourceLetters = countLetters(this.source)
    }

    fun match(candidates: List<String>): Set<String> {
        return candidates
                .filter { matches(it) }
                .toSet()
    }

    fun countLetters(word: String): Map<Char, Int> {
        return word.fold(mutableMapOf<Char, Int>()) {
            acc: MutableMap<Char, Int>, letter: Char ->
            acc.put(letter, acc.getOrDefault(letter, 0) + 1)
            acc
        }
    }

    fun matches(input: String): Boolean {
        val word = input.toLowerCase()
        return word != source && countLetters(word) == sourceLetters
    }
}
