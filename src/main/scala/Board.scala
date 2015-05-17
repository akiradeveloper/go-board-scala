import scala.collection.mutable

sealed trait Color
final case object Black extends Color
final case object White extends Color
final case object Gray extends Color
final case object Empty extends Color
final case object Outside extends Color

class Board(_n:Int) {
  type Pos = (Int, Int)

  val n = _n
  val matrix = Array.fill[Color](n+2, n+2)(Outside)
  for (i <- 1 to n) {
    for (j <- 1 to n) {
      matrix(i)(j) = Empty
    }
  }
  var kou = None
  val agehama = Array.fill[Int](2)(0)

  def size = n  

  def show {
    print("kou:"); println(kou)
    printf("agehama:%d,%d\n", agehama(0), agehama(1))
    for (i <- 1 to n) {
      for (j <- 1 to n) {
        printf("%c ", matrix(i)(j) match {
          case Empty => '+'
          case _ => assert(false)
        })
      }
      print("\n")
    }
  }

  def neighbours(i:Int, j:Int) = {
    Seq((i,j-1), (i,j+1), (i-1,j), (i+1,j))
  }

  def outBoard(i:Int, j:Int) = {
    i<=0 || i>size || j<=0 || j>=size
  }

  def removeStones(xs: Seq[Pos]) {
    xs foreach { case (i, j) => matrix(i)(j) == Empty }
  }

  def searchHoles(i:Int, j:Int) = {
    neighbours(i, j) exists { case (i, j) => matrix(i)(j) == Empty }
  }

  // after put c'
  // c: color to remove
  def removeList(i:Int, j:Int, c:Color) = {
    var found_hole = false
    var visited_pos = mutable.Set[Pos]()
    var lis = mutable.Seq[Pos]()
    def visit(i:Int, j:Int, c:Color) {
      if (found_hole) { return }
      if (outBoard(i, j)) { return }
      if (matrix(i)(j) != c) { return }
      if (visited_pos((i, j))) { return }
      if (searchHoles(i, j)) {
        found_hole = true
        return
      }
      lis = lis :+ (i, j)
      visited_pos.add((i, j))
      neighbours(i, j) foreach { case (i, j) => visit(i, j, c) }
    }
    visit(i, j, c)
    if (found_hole) Seq() else lis
  }

  // after put c
  // the returned list may contain duplicated elements
  def removeListByPut(i:Int, j:Int, c:Color) = {
    val c_ = c match {
      case Black => White
      case White => Black
    }
    neighbours(i, j)
    .map { case (i, j) => removeList(i, j, c_) }
    .flatten
  }

  // before put c
  def tryRemoveList(i:Int, j:Int, c:Color) = {
    require(matrix(i)(j) == Empty)
    var r = Seq[Pos]()
    matrix(i)(j) = c
    r = removeListByPut(i, j, c)
    matrix(i)(j) = Empty
    r
  }
}

object Board {
  def main(args:Array[String]) {
    val b = new Board(5)
    b.show
  }
}
