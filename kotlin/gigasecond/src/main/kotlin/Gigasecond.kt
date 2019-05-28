import java.time.LocalDate
import java.time.LocalDateTime

class Gigasecond {
  public val date: LocalDateTime
  val giga: Long = 1_000_000_000

  constructor(moment: LocalDateTime) {
    date = moment.plusSeconds(giga)
  }

  constructor(moment: LocalDate) : this(moment.atStartOfDay())
}
