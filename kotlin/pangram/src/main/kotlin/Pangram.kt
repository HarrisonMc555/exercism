object Pangram {
  val alphabet: String = "abcdefghijklmnopqrstuvwxyz"

  fun isPangram(input: String): Boolean {
    val letters = input.toLowerCase().toSet()
    return alphabet.all { letters.contains(it) }
  }
}
