class Squares(val number: Int) {
    public fun squareOfSum(): Int {
        val summed = (1..this.number).sum()
        return summed * summed
    }

    public fun sumOfSquares(): Int {
        return (1..this.number).map { it * it }.sum()
    }

    public fun difference(): Int {
        return squareOfSum() - sumOfSquares()
    }
}
