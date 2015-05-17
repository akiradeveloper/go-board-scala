import go._
import scala.collection.mutable
import org.scalatest._

class RemoveTest extends FunSuite {
  def run(input: Seq[Board.Put], result: Seq[Board.Put]) = {
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
  test("1") {
    run(
      Seq((10,10,Black),(10,9,White),(10,11,White),(9,10,White),(11,10,White)),
      Seq((10,9,White),(10,11,White),(9,10,White),(11,10,White))) 
  }
  test("2") {
    run(
      Seq((10,9,White),(10,11,White),(9,10,White),(10,10,Black)),
      Seq((10,9,White),(10,11,White),(9,10,White),(10,10,Black)))
  }
  test("3") {
    run(
      Seq((9,1,Black),(8,1,Black),(7,1,White),(10,1,White),(8,2,White),(9,1,White)),
      Seq((7,1,White),(10,1,White),(8,2,White),(9,1,White)))
  }
  test("4") {
    run(
      Seq((9,1,Black),(8,1,White),(10,1,White),(9,2,White)),
      Seq((8,1,White),(10,1,White),(9,2,White)))
  }
  test("5") {
    run(
      Seq((10,10,Black),(11,10,Black),(9,10,White),(10,9,White),(10,11,White),(11,9,White),(11,11,White),(12,10,White)),
      Seq((9,10,White),(10,9,White),(10,11,White),(11,9,White),(11,11,White),(12,10,White)))
  }
  test("6") {
    run(
      Seq((2,1,Black),(2,2,White),(2,3,Black),(1,1,White),(1,4,Black),(1,3,White),(1,2,Black)),
      Seq((2,1,Black),(2,2,White),(2,3,Black),(1,4,Black),(1,2,Black)))
  }
}
