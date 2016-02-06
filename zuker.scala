class Main {
  val size = 10
  val matrix_w = for {
    x <- (1 to size).toList
  } yield for {
    y <- (1 to size).toList
  } yield (x, y)

  val matrix_v = for {
    x <- (1 to size).toList
  } yield for {
    y <- (1 to size).toList
  } yield (x, y)
  val length = 0
  val sum = 0
  val numbers: scala.collection.immutable.Map[java.lang.String, Array[java.lang.String]] = Map("Phe" -> Array[java.lang.String]("UUA", "UUC", "UUG", "UUU"))
  numbers.get("Phe")

  def sumInts(x: java.lang.String, y: java.lang.String): Boolean =
    if (((x == "A") && (y == "U")) || ((x == "U") && (y == "A")) || ((x == "G") && (y == "C")) || ((x == "C") && (y == "G"))) true else false
}
