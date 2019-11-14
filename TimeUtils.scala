import java.time.{DateTimeException, Instant}
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.time.temporal.ChronoUnit

import shapeless.tag
import shapeless.tag.@@

object TimeUtils {

  private val strictInstantFormatter: DateTimeFormatter = new DateTimeFormatterBuilder().appendInstant(3).toFormatter

  val epochMillisOnly: Instant @@ MillisOnly = truncateToMillis(Instant.EPOCH)

  // Marker trait for an Instant that is truncated to millisecond precision
  sealed trait MillisOnly

  /** A strict Instant parser that ensures */
  def formatInstantStrict(instant: Instant @@ MillisOnly): String = strictInstantFormatter.format(instant)
  def unsafeParseInstantStrict(str: String): Instant @@ MillisOnly =
    try unsafeCheckMillisOnly(Instant.from(strictInstantFormatter.parse(str)))
    catch {
      case e: DateTimeException =>
        throw new IllegalArgumentException(
          "string must be of the form 1000-01-01T00:00:00.000Z (Millisecond component must exist)",
          e
        )
    }

  def truncateToMillis(instant: Instant): Instant @@ MillisOnly =
    tag[MillisOnly](instant.truncatedTo(ChronoUnit.MILLIS))

  def unsafeCheckMillisOnly(instant: Instant): Instant @@ MillisOnly = {
    require(instant.getNano % 1000000 == 0, s"Instant must not have precision beyond millisecond level $instant")
    tag[MillisOnly](instant)
  }

}
