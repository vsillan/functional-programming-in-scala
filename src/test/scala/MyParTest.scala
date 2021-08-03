import scala_book._
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

class MyParTest extends UnitSpec {
  "parFilter" should "work" in {
    val parFilter = Par.parFilter(List(1, 2, 3, 4))(a => a % 2 == 0)
    val pool: ExecutorService = Executors.newFixedThreadPool(3)
    val result = parFilter(pool).get()

    assert(result == List(2, 4))
  }

  "sumOfWords" should "work" in {
    val par = Par.sumOfWords(List("first paragraph", "second paragraph"))
    val pool: ExecutorService = Executors.newFixedThreadPool(3)
    val result = par(pool).get()

    assert(result == 4)
  }
}
