package go

import scala.collection.mutable

sealed trait Color
final case object Black extends Color
final case object White extends Color
final case object Empty extends Color
final case object Outside extends Color
final case object Dame extends Color

class Board(_n:Int) {
  import go.Board._

  val n = _n
  val matrix = Array.fill[Color](n+2, n+2)(Outside)
  for (i <- 1 to n) {
    for (j <- 1 to n) {
      matrix(i)(j) = Empty
    }
  }
  var kou: Option[Pos] = None
  val agehama = Array.fill[Int](2)(0)

  def get(i:Int, j:Int) = { matrix(i)(j) }

  private def exists(i:Int, j:Int) = {
    matrix(i)(j) != Empty
  }

  private def bw2int(c:Color): Int = {
    (c: @unchecked) match {
      case Black => 0
      case White => 1
    }
  }

  def size = n  

  def show {
    print("kou:"); println(kou)
    printf("agehama:%d,%d\n", agehama(0), agehama(1))
    for (i <- 1 to n) {
      for (j <- 1 to n) {
        printf("%c ", matrix(i)(j) match {
          case Empty => '+'
          case Black => '@'
          case White => 'O'
          case Dame => 'x'
          case _ => assert(false)
        })
      }
      print("\n")
    }
  }

  private def neighbours(i:Int, j:Int) = {
    Seq((i,j-1), (i,j+1), (i-1,j), (i+1,j))
  }

  private def outBoard(i:Int, j:Int) = {
    i<=0 || i>size || j<=0 || j>=size
  }

  private def removeStones(xs: Seq[Pos]) {
    xs foreach { case (i, j) => matrix(i)(j) = Empty }
  }

  private def searchHoles(i:Int, j:Int) = {
    neighbours(i, j) exists { case (i, j) => matrix(i)(j) == Empty }
  }

  // after put c'
  // c: color to remove
  private def removeList(i:Int, j:Int, c:Color) = {
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
  private def removeListByPut(i:Int, j:Int, c:Color) = {
    val c_ = (c: @unchecked) match {
      case Black => White
      case White => Black
    }
    neighbours(i, j)
    .map { case (i, j) => removeList(i, j, c_) }
    .flatten
  }

  // before put c
  private def tryRemoveList(i:Int, j:Int, c:Color) = {
    require(matrix(i)(j) == Empty)
    var r = Seq[Pos]()
    matrix(i)(j) = c
    r = removeListByPut(i, j, c)
    matrix(i)(j) = Empty
    r
  }

  private def willTakeOne(p: Put): Boolean = {
    val (i, j, c) = p
    tryRemoveList(i, j, c).length == 1
  }

  private def trySuicide(p: Put): Seq[Pos] = {
    val (i, j, c) = p
    var r = Seq[Pos]()
    matrix(i)(j) = c
    r = removeList(i, j, c)
    matrix(i)(j) = Empty
    r 
  }

  private def isKouSuicide(p: Put) = {
    trySuicide(p).length == 1
  }

  def canPut(p: Put): Boolean = {
    val (i, j, c) = p
    if (exists(i, j)) { return false }
    val needsKouDate = kou match {
      case Some((i_, j_)) => (i, j) == (i_, j_)
      case None => false
    }
    if (needsKouDate) { return false }
    val isSuicide = trySuicide(p).length > 0
    if (isSuicide && !isKouSuicide(p)) { return false }
    true
  }

  def pass {
    kou = None
  }

  private def willKouTake(p: Put) = {
    isKouSuicide(p) && willTakeOne(p)
  }

  def putStone(p: Put) {
    val (i, j, c) = p
    val wasKouTake = willKouTake(p)
    matrix(i)(j) = c
    val xs = removeListByPut(i, j, c)
    agehama(bw2int(c)) += xs.length
    removeStones(xs)
    if (wasKouTake) kou = Some(xs.head) else kou = None
  }

  // def putStones(xs: Seq[Pos]) {
  //   val puts = xs zip List(Black, White).toStream
  //   puts foreach { case ((i, j), c) => putStone((i, j, c)) }
  // }
}

object Board {
  type Pos = (Int, Int)
  type Put = (Int, Int, Color)
  def main(args:Array[String]) {
    val b = new Board(5)
    b.show
  }
}
