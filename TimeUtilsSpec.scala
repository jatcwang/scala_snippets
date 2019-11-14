import java.time.Instant

import TimeUtils._
import org.scalatest.{Matchers, WordSpec}

class TimeUtilsSpec extends WordSpec with Matchers {

  val noMillis = unsafeCheckMillisOnly(Instant.parse("2019-01-25T14:25:12Z"))
  val hasMillis = unsafeCheckMillisOnly(Instant.parse("2019-01-25T13:12:32.035Z"))

  "formatInstantStrict" should {
    "format an instant without millis component to still have .000Z at the end" in {
      formatInstantStrict(noMillis) shouldBe "2019-01-25T14:25:12.000Z"
    }

    "format an instant with millis component" in {
      formatInstantStrict(hasMillis) shouldBe "2019-01-25T13:12:32.035Z"
    }
  }

  "unsafeParseInstantStrict" should {
    val expectedErrorMsg = "string must be of the form 1000-01-01T00:00:00.000Z (Millisecond component must exist)"
    "fail to parse an invalid instant" in {
      val e = intercept[IllegalArgumentException] {
        unsafeParseInstantStrict("2019-01-25.14:25:12.000Z")
      }
      e.getMessage shouldBe expectedErrorMsg
    }

    "Fail to parse an instant without millis component" in {
      val e = intercept[IllegalArgumentException] {
        unsafeParseInstantStrict("2019-01-25T14:25:12Z")
      }
      e.getMessage shouldBe expectedErrorMsg
    }

    "Successfully parse an instant with millis component" in {
      unsafeParseInstantStrict("2019-01-25T13:12:32.035Z") shouldBe hasMillis
    }

    "Fail to parse an instant with nonas component" in {
      val e = intercept[IllegalArgumentException] {
        unsafeParseInstantStrict("2019-01-25T14:25:12.035012Z")
      }
      e.getMessage shouldBe expectedErrorMsg

      val e2 = intercept[IllegalArgumentException] {
        unsafeParseInstantStrict("2019-01-25T14:25:12.035000Z")
      }
      e2.getMessage shouldBe expectedErrorMsg
    }
  }

  "unsafeCheckMillisOnly" should {
    "fail if Instant has precision beyond milliseconds" in {
      val input = Instant.parse("2019-01-25T14:25:12.0351Z")
      val e = intercept[IllegalArgumentException] {
        unsafeCheckMillisOnly(input)
      }
      e.getMessage shouldBe "requirement failed: Instant must not have precision beyond millisecond level 2019-01-25T14:25:12.035100Z"
    }

    "succeed if precision does not exceed millisecond level" in {
      unsafeCheckMillisOnly(Instant.parse("2019-01-25T14:25:12.035Z")) shouldBe Instant.parse(
        "2019-01-25T14:25:12.035Z"
      )
    }
  }

  "truncateToMillis" should {
    "truncate an instant to millisecond precision" in {
      truncateToMillis(Instant.parse("2019-01-25T14:25:12.035010Z")) shouldBe Instant.parse("2019-01-25T14:25:12.035Z")
    }

    "handle an instant that's already at millisecond precision" in {
      truncateToMillis(Instant.parse("2019-01-25T14:25:12.035Z")) shouldBe Instant.parse("2019-01-25T14:25:12.035Z")
    }
  }
}
