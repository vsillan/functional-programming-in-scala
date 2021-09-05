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

  "fork" should "not affect the result" in {
    val a = Par.map(Par.unit(3))(_ + 1)
    val b = Par.fork(a)

    val pool: ExecutorService = Executors.newFixedThreadPool(2)
    assert(a(pool).get() == b(pool).get())
  }

  "sumOfWords" should "work" in {
    val par = Par.sumOfWords(List("first paragraph", "second paragraph"))
    val pool: ExecutorService = Executors.newFixedThreadPool(3)
    val result = par(pool).get()

    assert(result == 4)
  }

  "choiceN" should "choose based on the result of the first arg" in {
    val x = Par.unit(2)

    val a = Par.unit(1)
    val b = Par.unit(2)
    val c = Par.unit(3)

    val par = Par.choiceN(x)(List(a, b, c))
    val pool: ExecutorService = Executors.newFixedThreadPool(3)
    val result = par(pool).get()

    assert(result == 3)
  }

  "genericChoice" should "choose based on the result of the first arg" in {
    val x = Par.unit(false)

    val a = Par.unit(1)
    val b = Par.unit(2)

    val par = Par.choiceViaGenericChoice(x)(a, b)
    val pool: ExecutorService = Executors.newFixedThreadPool(3)
    val result = par(pool).get()

    assert(result == 2)
  }
}
