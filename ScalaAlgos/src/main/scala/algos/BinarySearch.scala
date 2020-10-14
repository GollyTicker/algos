package algos

object BinarySearch {
  // general binary search. given a range [from,until) (excluding to)
  // and a predicate that switches from true to false atmost
  // once from "from" until "until".
  // e.g. pred([from,until-1]) = T T ... T T F F ... F F
  // Though it's possible, that preds == true or preds == false constantly.
  // Returns the largest i so that pred(i) == true or None, if it doesn't exist.
  def binarySearch[A:Integral](from:A,until:A,pred:A => Boolean):Option[A] = {
    val num = implicitly[Integral[A]]
    import num._
    // base case, None
    if (!pred(from))  None
    else if (plus(from,one) >= until) {  // at least one T exists. base case
      if (pred(from)) Some(from) else None
    }
    else {  // check whether the middle one is T
      val m = quot( plus(from,until), plus(one,one) )
      if (pred(m)) binarySearch(m,until,pred)
      else binarySearch(from,m,pred)
    }
  }
  /*
  Example:
  val arr=Array(0,1,2,3,4,5,6,7)
  binarySearch[Int](0,arr.length,(x:Int) => x <= 4) // => 4 (index)
  */
}
