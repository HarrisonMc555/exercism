class Year(year: Int) {
    val isLeap: Boolean = isLeapYear(year)

    fun isLeapYear(year: Int): Boolean {
        return year.isDivisibleBy(4) &&
        year.isNotDivisibleBy(100) ||
        year.isDivisibleBy(400)
    }

    fun Int.isDivisibleBy(divisor: Int): Boolean {
        return rem(divisor) == 0
    }

    fun Int.isNotDivisibleBy(divisor: Int): Boolean {
        return rem(divisor) != 0
    }
}
