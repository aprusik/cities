import doodle.core._
import doodle.syntax._

val foo = (Point(0,1), Vec(1, -1))
val bar = (Point(0, 0), Vec(1, 1))

def intersects(s1: (Point, Vec), s2: (Point, Vec)): Option[Point] = {
  val t = (s2._1 - s1._1) cross s2._2 / (s1._2 cross s2._2)
  val u = (s2._1 - s1._1) cross s1._2 / (s1._2 cross s2._2)
  if ((s1._2 cross s2._2) != 0 && (0<=t && t<=1) && (0<=u && u<=1))
    Some(s1._1 + (s1._2 * t))
  else None
}

intersects(foo, bar)