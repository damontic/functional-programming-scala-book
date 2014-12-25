package co.damontic.chapter2.isSorted

object IsSorted {
  def isSorted[A](asArray: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(index: Int): Boolean = {
      if(index == asArray.length-1) true
      else if (ordered(asArray(index), asArray(index + 1))) loop(index + 1)
      else false
    }
    loop(0)
  }
}
