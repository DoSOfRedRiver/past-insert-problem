package dosofredriver

import scala.annotation.tailrec

/**
  * Created by dosof on 10.06.2017.
  */
object Solution {
  type Id = Long
  case class Timestamp(day: Int, month: Int, year: Int) {
    def < (right: Timestamp): Boolean = {
      year < right.year ||
      (year == right.year && month < right.month) ||
      (year == right.year && month == right.month && day < right.day)
    }
  }
  case class Repr(id: Id, timestamp: Timestamp)


  def solve(reprs: List[Repr]): List[Id] = {
    @tailrec
    def helper(l: List[Repr], prevId: Id, maxTimestamp: Timestamp, acc: List[Id]): List[Id] = l match {
      case x :: xs =>
        if (isPastInsert(prevId, x.id, maxTimestamp, x.timestamp))
          helper(xs, x.id, max(x.timestamp, maxTimestamp), x.id +: acc)
        else
          helper(xs, x.id, max(x.timestamp, maxTimestamp), acc)

      case Nil =>
        acc
    }

    reprs match {
      case x :: xs => helper(xs, x.id, x.timestamp, List.empty[Id])
      case _ => List.empty[Id]
    }
  }

  def max(l: Timestamp, r: Timestamp): Timestamp = if (l < r) r else l

  /*
    ID(n) > ID(n-1)
    Timestamp(n) < max(timestamp(1):timestamp(n-1))
  */
  private def isPastInsert(prevId: Id, id: Id, maxTimestamp: Timestamp, timestamp: Timestamp) = {
    id > prevId && timestamp < maxTimestamp
  }
}
