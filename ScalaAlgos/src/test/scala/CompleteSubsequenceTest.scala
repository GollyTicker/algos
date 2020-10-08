import org.scalatest.FunSuite
import algos.CompleteSubsequence.{naiveAlgorithm, slidingWindow}

class CompleteSubsequenceTest extends FunSuite {
  def foreachGen[B](f:Seq[Int] => B):Unit ={
    import util.Random
    Random.setSeed(0)
    val k = 5
    val maxSize = 7
    val n = 100
    for (size <- 0 to maxSize){
      for (_ <- 1 to n){ // n times
        // create array and test
        val ints = (1 to size).toArray
        for (i <- 0 until size) {
          ints(i) = Random.nextInt(k)
        }
        f(ints.toSeq)
      }
    }
    ()
  }


  test("doesn't crash") {
    foreachGen(x => naiveAlgorithm(x))
  }

  test("model checking") {
    foreachGen(x => naiveAlgorithm(x) == slidingWindow(x))
  }
}