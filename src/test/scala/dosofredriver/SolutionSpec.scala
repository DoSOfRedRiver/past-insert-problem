package dosofredriver

import dosofredriver.Solution.{Id, Repr, Timestamp, solve}
import org.scalatest.{FlatSpec, Matchers, WordSpecLike}

/**
  * Created by dosof on 10.06.2017.
  */
class SolutionSpec extends Matchers with WordSpecLike {

  "A Solution" should {

    "provide ids of past-inserted representations" in {
      val pastInserted = List[Id](5, 6)

      val listFromEmail = List(
        Repr(1, Timestamp(11, 9, 2016)),
        Repr(2, Timestamp(12, 9, 2016)),
        Repr(3, Timestamp(13, 9, 2016)),
        Repr(4, Timestamp(14, 9, 2016)),
        Repr(5, Timestamp(9, 9, 2016)),
        Repr(6, Timestamp(8, 9, 2016)),
        Repr(7, Timestamp(15, 9, 2016))
      )

      solve(listFromEmail) should contain theSameElementsAs pastInserted
    }

    "return empty list on empty argument list" in {
      solve(List.empty[Repr]) shouldBe empty
    }
  }
}
