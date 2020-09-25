import Shapez._
import org.scalatest.FunSuite

class ColorsTest extends FunSuite {
  def colors = Array(Red, Green, Blue, Yellow, Purple, Cyan, White, Uncolored)

  test("basic colors add correctly") {
    assert(Red + Green == Yellow)
    assert(Red + Blue == Purple)
    assert(Green + Red == Yellow)
    assert(Green + Blue == Cyan)
    assert(Blue + Green == Cyan)
    assert(Blue + Red == Purple)
  }
  test("adding a color to itself returns the same color") {
    colors.foreach(color => assert(color + color == color))
  }
  test("adding a color to uncolored returns the original color") {
    colors.foreach(color => assert(Uncolored + color == color))
  }
  test("adding a color to white always returns white") {
    colors.foreach(color => assert(White + color == White))
  }
  test("advanced colors add correctly") {
    assert(Yellow + Red == Yellow)
    assert(Yellow + Green == Yellow)
    assert(Yellow + Blue == White)
    assert(Yellow + Purple == White)
    assert(Yellow + Cyan == White)

    assert(Purple + Red == Purple)
    assert(Purple + Blue == Purple)
    assert(Purple + Green == White)
    assert(Purple + Yellow == White)
    assert(Purple + Cyan == White)

    assert(Cyan + Red == White)
    assert(Cyan + Blue == Cyan)
    assert(Cyan + Green == Cyan)
    assert(Cyan + Yellow == White)
    assert(Cyan + Purple == White)
  }
}