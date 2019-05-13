import scala.collection.mutable

import doodle.syntax._
import doodle.image._
import doodle.core._
import doodle.core.PathElement._
import doodle.image.syntax._

import scala.util.Random

/** Procedurally generates city maps as a road network. */
object CitiesApp {
  //================ CONSTANTS ================
  // Size of each road segment
  val GrowDist: Double = 5
  // Number of road segments to generate
  val MaxSegs: Int = 10000
  // Max deviation from 0 degrees a road segment should be able to turn
  val MaxAngle: Int = 10
  // Probability (out of 1) that a branch will be created
  val BranchProb: Double = 0.05
  val StreetBranchProb = 0.5
  // Seed for random number generator
  val Seed: Int = 10
  //
  val SnapDist: Double = 3
  //
  val NoiseScale: Double = 100

  //============== CASE CLASSES ===============
  /** A road segment.
    *
    * @param time order of creation (starting at 0)
    * @param seg coordinate segment of road
    * @param meta metadata
    */
  case class road(time: Int, seg: segment, meta: roadMeta)
  /** Factory for [[road]] instances. */
  object road {
    /** Creates a road at an offset from a segment.
      *
      * @param seg segment to extend road from
      * @param off degrees of offset from the randomly determined angle
      * @return a new road starting at the end of segment and angled in the
      *         direction of a randomly determined angle between 0 and
      *         [[MaxAngle]] + the offset.
      */
    def apply(seg: segment, off: Int): road = {
      var possSegs: List[segment] = Nil
      possSegs = segment(seg.end,
        Vec.polar(GrowDist, off.degrees + seg.v.angle)) +: possSegs
      possSegs = segment(seg.end,
        Vec.polar(GrowDist, off.degrees + seg.v.angle +
        (rand.nextInt(MaxAngle*2) - MaxAngle).degrees)) +: possSegs
      road(0, possSegs.maxBy(s => getNoise(s.end)), roadMeta())
    }
  }

  /** Road metadata
    *
    * @param ended true if the road should no longer have new segments generated
    */
  case class roadMeta(ended: Boolean = false, color: Int = 0, street: Boolean = false)

  /** A line segment defined by a point and a vector
    *
    * @param p starting point of segment
    * @param v vector to extend from [[p]]
    */
  case class segment(p: Point, v: Vec) {
    /** Returns a string of the format: "[start point] [end point]". */
    override def toString: String = s"${p.toString} $end"
    /** Returns the result of adding [[v]] to [[p]] */
    def end: Point = p + v
    /** Returns a new segment with the length multiplied by x. */
    def *(x: Double): segment = segment(p, v * x)
  }

  //================ VARIABLES ================
  // Queue for temporarily holding unprocessed road segments
  val q: mutable.PriorityQueue[road] = mutable.PriorityQueue[road](
    road(0, segment(Point.zero, Vec.zero), roadMeta())
  )(Ordering.by(-_.time))

  // List of all line segments in the road network generated so far
  var segs: List[road] = Nil

  // Set of all intersection points
  var points: KdTree2 = new KdTree2

  // Random number generator
  val rand = new Random(Seed)

  val noise1: OpenSimplexNoise = new OpenSimplexNoise(Seed)
  val noise2: OpenSimplexNoise = new OpenSimplexNoise(Seed)
  val noise3: OpenSimplexNoise = new OpenSimplexNoise(Seed)

  var maxX: Int = 0
  var maxY: Int = 0
  var minX: Int = 0
  var minY: Int = 0

  //================= METHODS =================
  def updateBounds(p: Point): Unit = {
    if (p.x > maxX) maxX = p.x.toInt
    if (p.x < minX) minX = p.x.toInt
    if (p.y > maxY) maxY = p.y.toInt
    if (p.y < minY) minY = p.y.toInt
  }

  def getNoise(p: Point): Double = {
    noise1.eval(p.x / NoiseScale, p.y / NoiseScale, 0.0) *
    noise2.eval(p.x / NoiseScale, p.y / NoiseScale, 0.0) *
    noise3.eval(p.x / NoiseScale, p.y / NoiseScale, 0.0)
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
    else if ((s1.p == s2.p && s1.end == s2.end) || (s1.p == s2.end && s1.end == s2.p))
      Some(s1.p)
    else None
  }

  /** Checks the given road for conformity to local constraints, and modifies
    *
    * the road with the provided function if necessary.
    * @param r a road to apply local constraints to
    * @param f a function to modify the given road value
    * @return true if the local constraints can be satisfied, false otherwise
    */
  def localConstraints(r: road, f: Function[road, Unit]): Boolean = {
    // Check for nearby crossings
    if (points.size > 0) {
      val near = points.nearestTo(r.seg.end, 10)
      val dist = (near(0)._1 - r.seg.end).length
      if (dist <= SnapDist && dist >= 0.0000001) {
        f(road(
          r.time,
          segment(r.seg.p, near(0)._1 - r.seg.p),
          roadMeta(ended = true, 0, street = r.meta.street)
        ))
      }
      else {
        // check for intersection
        near.exists(n => {
          val s = n._2
          val inter = intersects(r.seg * 2, s)
          if (inter.isDefined) {
            if (inter.get != s.p && inter.get != s.end)
              f(road(
                r.time,
                segment(r.seg.p, inter.get - r.seg.p),
                roadMeta(ended = true, 0, street = r.meta.street))
              )
          }
          inter.isDefined
        })
      }
    }
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
    if (!meta.ended) {
      var newSegs =
        if (!meta.street)
          List(road(seg, 0))
        else {
          if (getNoise(seg.p) < 0)
            List(road(
              0,
              segment(seg.end, Vec.polar(GrowDist, seg.v.angle)),
              roadMeta(street = true)
            ))
          else
            List.empty
        }
      if (rand.nextDouble() < BranchProb && getNoise(seg.p) < 0 && !meta.street) {
        if (rand.nextInt(100) < 50)
          newSegs = road(seg, 90) +: newSegs
        else
          newSegs = road(seg, -90) +: newSegs
      }
      if (rand.nextDouble() < StreetBranchProb && getNoise(seg.p) < 0) {
        if (rand.nextInt(100) < 50)
          newSegs = road(
            0,
            segment(seg.end, Vec.polar(GrowDist, 90.degrees +
              rand.nextInt(MaxAngle).degrees + seg.v.angle)),
            roadMeta(street = true)
          ) +: newSegs
        else
          newSegs = road(
            0,
            segment(seg.end, Vec.polar(GrowDist, -90.degrees -
              rand.nextInt(MaxAngle).degrees + seg.v.angle)),
            roadMeta(street = true)
          ) +: newSegs
      }
      newSegs
    }
    else
      List[road]()
  }

  /** Creates a window displaying a procedurally generated road network. */
  def main(args: Array[String]): Unit = {
    // Generate segments
    while (q.nonEmpty && segs.size <= MaxSegs) {
      var rd: road = q.dequeue()
      val accepted: Boolean = localConstraints(rd, x => rd = x)
      if (accepted) {
        segs = rd +: segs
        points.insert(rd.seg.p, rd.seg)
        updateBounds(rd.seg.p)
        for (r <- globalGoals(rd.seg, rd.meta)) {
          q.enqueue(road(r.time + 1 + rd.time, r.seg, r.meta))
        }
      }
    }

    // Convert Segments to Paths
    val drawSegs = segs.map(s => {
      val i = Image.openPath(List(
        moveTo(s.seg.p),
        lineTo(s.seg.end + s.seg.v)
      )).at(s.seg.p.x, s.seg.p.y)
      if (s.meta.color == 1) i.strokeColor(Color.red)
      else if (!s.meta.street) i.strokeColor(Color.blue)
      else if (s.meta.color == 2) i.strokeColor(Color.blue)
      else if (getNoise(s.seg.p) > 0) i.strokeColor(Color.green)
      else i
    })

    // Reduce path List to single Image
    var drawable =
      if (drawSegs.length / 700 >= 2) {
        // if there are too many segments, divide and conquer
        val n = drawSegs.length / 700
        var parts: List[Image] = List.empty
        for (i <- 1 to n) {
          parts = drawSegs.slice(
            (drawSegs.length/n)*(i-1), (drawSegs.length/n)*i
          ).reduce((x, y) => x on y) +: parts
        }
        parts.reduce((x, y) => x on y)
      }
      else {
        // or just reduce it if it's small enough
        drawSegs.reduce((x, y) => x on y)
      }

//    var population: Image = Image.empty
//    for (x <- minX*2 to maxX*2 by 50;
//         y <- minY*2 to maxY*2 by 50) {
//      val square = Image.square(50).at(x, y).fillColor(
//        if (getNoise(Point(x, y)) > 0)
//          Color.green.lighten(getNoise(Point(x, y)).normalized)
//        else
//          Color.blue
//      ).noStroke
//      population = square on population
//    }
//    drawable = drawable on population
    // Print Image
    import doodle.java2d._
    drawable.draw()
  }

}
