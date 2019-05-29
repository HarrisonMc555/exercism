object Raindrops {
    fun convert(input: Int): String {
        val factor_sounds = arrayOf(
            Pair(3, "Pling"),
            Pair(5, "Plang"),
            Pair(7, "Plong")
        )
        val to_keep = factor_sounds.filter { (x, _) -> input % x == 0 }
        val sounds = to_keep.map { (_, s) -> s }
        val combined = sounds.joinToString(separator = "")
        if (combined == "") {
            return input.toString()
        } else {
            return combined
        }
    }
}
