package algos


object CompleteSubsequence extends App {

  // input: a sequence of elements e.g. ABBCAAAB
  // output: the (0-based) indices of all smallest
  // subsequences in output that contains all elements in the subset.
  // e.g. in this case its only [(2,4)]

  def len(tpl:(Int,Int)):Int = (tpl._2+1)-tpl._1

  def naiveAlgorithm[A](a:Seq[A]):Set[(Int,Int)] = {
    import collection.mutable.{Map => MMap, Set => MSet}
    val all = a.toSet
    val solutions:MSet[(Int,Int)] = MSet.empty

    var i:Int = 0

    while (i < a.length) {
      val mp:MMap[A,Int] = MMap.from(all.map(x => (x,0)))

      var j:Int = i
      var br = false
      while(j < a.length && !br) {
        // BODY:
        // println(s"Looking at (i,j) = ($i,$j) => (${a(i)},${a(j)})")
        mp(a(j)) = mp(a(j)) + 1
        val complete = mp.values.forall(_ >= 1)
        if(complete) {
          solutions.update((i,j),included = true)
          br = true
        }
        // END.
        j = j+1
      }

      i = i+1
    }

    // find minimum length
    val optLen = solutions.foldLeft(a.length){case (acc,x) => math.min(acc,len(x))}

    // only keep correct length solutions
    solutions.filter(len(_) == optLen).toSet
  }

  def slidingWindow[A](a:Seq[A]):Set[(Int,Int)] = {
    import collection.mutable.{Map => MMap, Set => MSet}
    val all = a.toSet

    var solutions:MSet[(Int,Int)] = MSet.empty
    var currOptLen = a.length+1
    def updatePotentialMinimum(i:Int,j:Int):Unit = {
      val l = len((i,j))
      if (l < currOptLen){
        currOptLen = l
        solutions = MSet.empty
      }

      if (l == currOptLen) {
        solutions.update((i,j),included = true)
      } else { /* noop */ }
    }

    var i:Int = 0
    var j:Int = 0
    val mp:MMap[A,Int] = MMap.from(all.map(x => (x,0)))

    if(a.length == 0){
      return Set.empty
    }

    mp(a(j)) = mp(a(j)) + 1
    var continue = true

    while (continue) {
      val complete = mp.values.forall(_ >= 1)
      if (complete) {
        updatePotentialMinimum(i,j)

        // move window tail
        mp(a(i)) = mp(a(i)) - 1
        i = i + 1
      } else {
        // move window head
        j = j + 1
        if(j >= a.length) {
          continue = false
        }
        else {
          mp(a(j)) = mp(a(j)) + 1
        }
      }
    }

    solutions.toSet
  }

  // main
  val example = "AAABBCCAAAABCCCBBBAAC".toSeq

  println(naiveAlgorithm(example))

  println(slidingWindow(example))
}
