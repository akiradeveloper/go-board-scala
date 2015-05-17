import go._
import scala.collection.mutable
import org.scalatest._
class BoardTest extends FunSuite {
  def putTest(input: Seq[Board.Put], result: Seq[Board.Put]) = {
    val b = new Board(19)
    input foreach (b.putStone(_))
    val s = mutable.Set[Board.Put]()
    for (i <- 1 to b.size) {
      for (j <- 1 to b.size) {
        val c = b.get(i, j)
        (c: @unchecked) match {
          case Black | White => s.add((i, j, c))
          case _ => ()
        }
      }
    }
    assert(s === result.toSet)
  }
  test("test1") {
    putTest(
      Seq((10,10,Black),(10,9,White),(10,11,White),(9,10,White),(11,10,White)),
      Seq((10,9,White),(10,11,White),(9,10,White),(11,10,White))) 
  }
}
