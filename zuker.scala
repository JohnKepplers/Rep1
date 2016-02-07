class Main {
  val size = 10
  val matrix_w = Array[Array[Integer]](Array[Integer](3, 4, 5), Array[Integer](1, 2, 6), Array[Integer](7, 8, 9))
  val matrix_v = Array[Array[Integer]](Array[Integer](3, 2, 1), Array[Integer](4, 5, 1), Array[Integer](2, 3, 2))
  val length = 0
  val sum = 0
  val numbers: scala.collection.immutable.Map[java.lang.String, Array[java.lang.String]] = Map("Phe" -> Array[java.lang.String]("UUA", "UUC", "UUG", "UUU"))
  numbers.get("Phe")

  def sumInts(x: java.lang.String, y: java.lang.String): Boolean =
    if (((x == "A") && (y == "U")) || ((x == "U") && (y == "A")) || ((x == "G") && (y == "C")) || ((x == "C") && (y == "G"))) true else false
}
