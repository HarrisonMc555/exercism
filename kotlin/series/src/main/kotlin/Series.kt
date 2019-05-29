object Series {
    fun slices(length: Int, string: String): List<List<Int>> {
        if (length > string.length) {
            throw IllegalArgumentException("length $length is longer than string $string")
        }
        val digits = string.map { digit ->
            val digitValue = Character.getNumericValue(digit)
            if (digitValue < 0) {
                throw IllegalArgumentException("$digit is not a valid digit")
            }
            digitValue
        }
        return digits.windowed(length)
    }
}
