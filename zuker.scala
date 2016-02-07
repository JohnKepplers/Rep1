import scala.collection.mutable.ArrayBuffer

/**
  * Created by Alexander on 07.02.2016.
  */
class Main {
  var matrix_w = ArrayBuffer[ArrayBuffer[Double]]()
  var matrix_v = ArrayBuffer[ArrayBuffer[Double]]()
  var length = 0
  var sum = 0
  var numbers: scala.collection.immutable.Map[java.lang.String, Array[java.lang.String]] = Map("Phe" -> Array[java.lang.String]("UUA", "UUC", "UUG", "UUU"))
  numbers.get("Phe")


  def sumInts(x: java.lang.String, y: java.lang.String): Boolean =
    if (((x == "A") && (y == "U")) || ((x == "U") && (y == "A")) || ((x == "G") && (y == "C")) || ((x == "C") && (y == "G"))) true else false


  def _matrix_v(f: java.lang.String => Unit, str: java.lang.String) = {
    var a = new Main()
    a.length = str.length()
    var helping_list = ArrayBuffer[Double]()
    for (i <- 0 to a.length - 1) {
      for (j <- 0 to a.length - 1) {
        helping_list += 8.4
      }
      a.matrix_v += helping_list
      helping_list = ArrayBuffer[Double]()

    }
    for (i <- 0 to a.length - 1) {
      for (j <- 0 to a.length - 1) {
        if ((i > j - 4) && (i < j)) a.matrix_v(i)(j) = 4.4
      }
    }
    for (i <- 0 to a.length - 1) {
      for (j <- 0 to a.length - 1) {
        if (i < j) {
          var my_list = ArrayBuffer[Double]()
          my_list += a.eh(i, j)
          my_list += a.es(i, j) + a.matrix_v(i + 1)(j - 1)
          my_list += a.VBI(i, j)
          var local_minimum = my_list(0)
          for (k <- 1 to my_list.length - 1) {
            if (local_minimum > my_list(k)) local_minimum = my_list(k)
          }
          a.matrix_v(i)(j) = local_minimum
        }
      }
    }
  }

  def eh(i: Integer, j: Integer): Double = {
    var b = new Main()
    if ((b.length > 4) && (b.length < 10)) 4.4
    else if ((b.length > 9) && (b.length < 15)) 5.3
    else if ((b.length > 14) && (b.length < 20)) 5.8
    else if ((b.length > 19) && (b.length < 25)) 6.1
    else if ((b.length > 24) && (b.length < 30)) 6.3
    else if (b.length > 29) 6.5
    else 0
  }

  def es(i: Integer, j: Integer): Double = -0.5

  def eb(i: Integer, j: Integer, a: Integer, b: Integer): Double = -0.5

  def VBI(i: Integer, j: Integer): Double = {
    var c = new Main()
    var local_minimum: Double = 100000.0
    for (a <- i + 1 to j - 2) {
      for (b <- i + 2 to j - 1) {
        if ((a < b) && (a - i + j - b > 2)) {
          var d = c.eb(i, j, a, b) + c.matrix_v(a)(b)
          if (local_minimum > d) local_minimum = d
        }
      }
    }
    local_minimum
  }

}

