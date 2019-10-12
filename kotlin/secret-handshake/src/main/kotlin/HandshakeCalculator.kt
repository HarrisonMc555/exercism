class HandshakeCalculator {
    companion object {
        fun calculateHandshake(input: Int): List<Signal> {
            var result = mutableListOf<Signal>()
            for ((i, signal) in Signal.values().withIndex()) {
                val bitFlag = 1 shl i
                if (input and bitFlag != 0) {
                    result.add(signal)
                }
            }
            if (input and 16 != 0) {
                result.reverse()
            }
            return result
        }
    }
}
