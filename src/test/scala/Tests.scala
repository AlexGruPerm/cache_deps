import cats.effect.{IO}
import munit.ScalaCheckEffectSuite
import org.scalacheck.Prop.{forAll,collect}

class CacheTests extends ScalaCheckEffectSuite {

  test("PB test 1") {
    val q = forAll { (m: Int, n: Int) => collect(m, n, m+n) { m + n != 37 } }
    q.check
  }

  test("tests 1") {
    IO(42).map(it => assertEquals(it, 42))
  }

  test("tests 2") {
    IO(43).map(it => assertEquals(it, 43))
  }

  test("tests can return IO[Unit] with assertions expressed via a map") {
    IO(42).map(it => assertEquals(it, 42))
  }

}
