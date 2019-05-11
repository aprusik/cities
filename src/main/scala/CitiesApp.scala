import scala.collection.mutable

import doodle.syntax._
import doodle.image._
import doodle.core._
import doodle.core.PathElement._
import doodle.image.syntax._

import scala.util.Random

/** Procedurally generates city maps as a road network. */
object CitiesApp {
  // Size of each road segment
  val GrowDist: Double = 5
  // Number of road segments to generate
  val MaxSegs: Int = 500
  // Max deviation from 0 degrees a road segment should be able to turn
  val MaxAngle: Int = 10
  // Probability (out of 1) that a branch will be created
  val BranchProb: Double = 0.07

  val rand = new Random()

  /** Represents a road segment with a time of creation, segment, and metadata */
  case class road(time: Int, seg: segment, meta: roadMeta)
  /** Represents a line segment with a point and vector. */
  case class segment(p: Point, v: Vec) {
    override def toString: String = s"${p.toString} $end"
    def end: Point = p + v
  }
  /** Represents road segment metadata. */
  case class roadMeta()

  // Queue for temporarily holding unprocessed road segments
  val q: mutable.PriorityQueue[road] = mutable.PriorityQueue[road](
    road(0, segment(Point.zero, Vec.zero), roadMeta())
  )(Ordering.by(-_.time))

  // List of all line segments in the road network generated so far
  var segs: List[segment] = Nil
  // Set of all intersection points
  var points: Set[Point] = Set.empty

  /** Checks the given road for conformity to local constraints, and modifies
    *
    * the road with the provided function if necessary.
    * @param r a road to apply local constraints to
    * @param f a function to modify the given road value
    * @return true if the local constraints can be satisfied, false otherwise
    */
  def localConstraints(r: road, f: Function[road, Unit]): Boolean = {
    // check for intersection
    segs.exists(s => {
      val inter = intersects(r.seg, s)
      if (inter.isDefined) {
        if (inter.get != s.p && inter.get != s.end)
        f(road(
          r.time,
          segment(r.seg.p, inter.get - r.seg.p),
          r.meta))
      }
      inter.isDefined
    })
    // right now there aren't any illegal areas, just modified ones, so just
    // return true.  This may change in the future though, so still return
    // boolean.
    true
  }

  /** Provides a List of new roads possible from any given segment
    *
    * @param seg segment to build off of
    * @param meta road metadata
    * @return
    */
  def globalGoals(seg: segment, meta: roadMeta): List[road] = {
    var newSegs = List(road(0, segment(
      seg.end,
      Vec.polar(GrowDist, seg.v.angle +
        (rand.nextInt(MaxAngle*2) - MaxAngle).degrees)),
      roadMeta()))
    if (rand.nextDouble() < BranchProb) {
      newSegs =
        road(0, segment(
        seg.end,
        Vec.polar(GrowDist, -90.degrees + seg.v.angle +
          (rand.nextInt(MaxAngle*2) - MaxAngle).degrees)),
        roadMeta()) +: newSegs
    }
    else if (rand.nextDouble() < BranchProb) {
      newSegs =
        road(0, segment(
          seg.end,
          Vec.polar(GrowDist, 90.degrees + seg.v.angle +
            (rand.nextInt(MaxAngle*2) - MaxAngle).degrees)),
          roadMeta()) +: newSegs
    }
    newSegs
  }

  /** Checks if two line segments intersect.
    *
    * @param s1 segment 1
    * @param s2 segment 2
    * @return If there is an intersection, an option containing the intersection
    *         point, otherwise None
    */
  def intersects(s1: segment, s2: segment): Option[Point] = {
    val t = (s2.p - s1.p) cross (s2.v / (s1.v cross s2.v))
    val u = (s2.p - s1.p) cross (s1.v / (s1.v cross s2.v))
    if ((s1.v cross s2.v) != 0 && (0<=t && t<=1) && (0<=u && u<=1)) {
      if ((s1.v * t).length < 0.0000001)
        None
      else
        Some(s1.p + (s1.v * t))
    }
    else None
  }

  /** Creates a window displaying a procedurally generated road network. */
  def main(args: Array[String]): Unit = {
    // Generate segments
    while (q.nonEmpty && segs.size <= MaxSegs) {
      var rd: road = q.dequeue()
      val accepted: Boolean = localConstraints(rd, x => rd = x)
      if (accepted) {
        segs = rd.seg +: segs
        points = points + rd.seg.p
        points = points + (rd.seg.p + rd.seg.v)
        for (r <- globalGoals(rd.seg, rd.meta)) {
          q.enqueue(road(r.time + 1 + rd.time, r.seg, r.meta))
        }
      }
    }

    // Convert Segments to Paths
    val drawSegs = segs.map(s => {
      Image.openPath(List(
        moveTo(s.p),
        lineTo(s.end + s.v)
      )).at(s.p.x, s.p.y)
    })

    /** Processes Image list as n separate Image Lists to avoid stack overflow. */
    def partition(l: List[Image], n: Int): List[Image] = {
      var parts: List[Image] = List.empty
      for (i <- 1 to n) {
        parts = l.slice((l.length/n)*(i-1), (l.length/n)*i).reduce((x, y) => x on y) +: parts
      }
      parts
    }

    // Reduce path List to single Image
    val drawable =
      if (drawSegs.length / 700 >= 2)
        partition(drawSegs, drawSegs.length / 700).reduce((x, y) => x on y)
      else
        drawSegs.reduce((x, y) => x on y)

    // Print Image
    import doodle.java2d._
    drawable.draw()
  }

}
