object Main {

  def main(args: Array[String]): Unit = {
    println(lazylist.LazyList.apply(1, 2, 3).scanRight(0)(_ + _).toList)
  }
}
