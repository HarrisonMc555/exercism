object Raindrops {
    fun convert(input: Int): String {
        val factorSounds = mapOf(
            3 to "Pling",
            5 to "Plang",
            7 to "Plong"
        )
        val combined = factorSounds
                .filter { (x, _) -> input % x == 0}
                .values
                .joinToString(separator = "")
        if (combined == "") {
            return input.toString()
        } else {
            return combined
        }
    }
}
