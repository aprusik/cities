import CitiesApp.segment
import KdTree2.{bDist, box, node}
import doodle.core._

import scala.util.Random

// https://www.cs.cmu.edu/~ckingsf/bioinfo-lectures/kdtrees.pdf
class KdTree2() {
  var n: Option[node] = None
  var size = 0

  def insert(p: Point, seg: segment): Unit = {
    size += 1 //counts duplicates
    n = Some(insert(p, seg, n))
  }

  def insert(p: Point, s: segment, n: Option[node], cd: Boolean = true): node = {
    var left: Option[node] = None
    var right: Option[node] = None
    var ret: node = node(p, left, right, s)
    if (n.isDefined) {
      if (n.get.loc == p) {
        left = n.get.left
        right = n.get.right
        size -= 1
      }
      else if ( if (cd) {p.x < n.get.loc.x} else {p.y < n.get.loc.y} ) {
        left = Some(insert(p, s, n.get.left, !cd))
        right = n.get.right
      }
      else {
        right = Some(insert(p, s, n.get.right, !cd))
        left = n.get.left
      }
      ret = node(n.get.loc, left, right, n.get.seg)
    }
    ret
  }

  var best: Vector[(Point, segment)] = Vector.empty
  var bestDist: Vector[Double] = Vector(Double.MaxValue)

  def nearestTo(p: Point, k: Int): Vector[(Point, segment)] = {
    best = Vector.empty
    bestDist = Vector(Double.MaxValue)
    nearest(p, k, n, box(Point(Int.MinValue, Int.MaxValue), Point(Int.MaxValue, Int.MinValue)))
  }


  def nearest(p: Point, k: Int, n: Option[node], b: box, cd: Boolean = true): Vector[(Point, segment)] = {
    if (n.isEmpty || (bDist(p, b) > bestDist.max && best.length >= k))
      best
    else {
      var dist = KdTree2.distance(p, n.get.loc)
      dist *= dist
      if (dist < bestDist.min) {
        best = (n.get.loc, n.get.seg) +: best
        bestDist = dist +: bestDist
        if (bestDist.length > k && best.length > k) {
          best = best.slice(0, k)
          bestDist = bestDist.slice(0, k)
        }
      }
      if ( if (cd) {p.x < n.get.loc.x} else {p.y < n.get.loc.y} ) {
        nearest(p, k, n.get.left, b.trimLeft(cd, n.get.loc), !cd)
        nearest(p, k, n.get.right, b.trimRight(cd, n.get.loc), !cd)
      }
      else {
        nearest(p, k, n.get.right, b.trimRight(cd, n.get.loc), !cd)
        nearest(p, k, n.get.left, b.trimLeft(cd, n.get.loc), !cd)
      }
    }
  }

}
object KdTree2 {
  case class node(loc: Point, left: Option[node], right: Option[node], seg: segment)
  case class box(c1: Point, c2: Point, loc: Point) {
    val width: Double = math.abs(c2.x - c1.x)
    val height: Double = math.abs(c1.y - c2.y)
    def trimLeft(cd: Boolean, p: Point): box = {
      if (cd) box(Point(p.x, c1.y), c2)
      else box(Point(c1.x, p.y), c2)
    }
    def trimRight(cd: Boolean, p: Point): box = {
      if (cd) box(c1, Point(p.x, c2.y))
      else box(c1, Point(c2.x, p.y))
    }
  }
  object box {
    def apply(c1: Point, c2: Point): box = {
      box(c1, c2, c2 + (c1-c2)*.5)
    }
  }

  def distance(p1: Point, p2: Point): Double = (p1 - p2).length

  // https://gamedev.stackexchange.com/questions/44483/how-do-i-calculate-distance-between-a-point-and-an-axis-aligned-rectangle
  def bDist(p: Point, b: box): Double = {
    val dx = math.max(math.abs(p.x - b.loc.x) - (b.width / 2), 0)
    val dy = math.max(math.abs(p.y - b.loc.y) - (b.height / 2), 0)
    dx * dx + dy * dy
  }

  //===========================================================

  def nearest2(p: Point, n: List[Point]): Point = {
    n.minBy(distance(_, p))
  }

  def main(args: Array[String]): Unit = {
    val rand = new Random(10)
    val points: KdTree2 = new KdTree2
    var ptsList: List[Point] = Nil
    for (_ <- 1 to 20000) {
      val newPt = Point(rand.nextInt(100), rand.nextInt(100))
      points.insert(newPt, segment(Point.zero, Vec.zero))
      ptsList = newPt +: ptsList
    }

    println(nearest2(Point(50, 50), ptsList))
    points.nearestTo(Point(50, 50), 10).foreach(println)
  }
}
