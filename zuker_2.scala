import scala.collection.mutable.ArrayBuffer
import java.util.Random

object Main extends App {
  val infinity = 99999
  var matrix_w = ArrayBuffer[ArrayBuffer[Double]]()
  var matrix_v = ArrayBuffer[ArrayBuffer[Double]]()
  var length = 0
  var sum: Double = 0
  val d: scala.collection.immutable.Map[ArrayBuffer[Char], Array[java.lang.String]] =
    Map(ArrayBuffer[Char]('P', 'h', 'e') -> Array[String]("UUA", "UUC", "UUG", "UUU"), ArrayBuffer[Char]('L', 'e', 'u') -> Array[String]("CUA", "CUC", "CUG", "CUU"),
      ArrayBuffer[Char]('I', 'l', 'e') -> Array[String]("AUA", "AUC", "AUU"), ArrayBuffer[Char]('M', 'e', 't') -> Array[String]("AUG"),
      ArrayBuffer[Char]('S', 'e', 'r') -> Array[String]("UCA", "UCC", "UCG", "UCU", "AGU", "AGC"), ArrayBuffer[Char]('P', 'r', 'o') -> Array[java.lang.String]("CCA", "CCC", "CCG", "CCU"),
      ArrayBuffer[Char]('U', 'h', 'r') -> Array[String]("ACA", "ACC", "ACG", "ACU"), ArrayBuffer[Char]('A', 'l', 'a') -> Array[java.lang.String]("GCA", "GCC", "GCG", "GCU"),
      ArrayBuffer[Char]('U', 'y', 'r') -> Array[String]("UAC", "UAU"), ArrayBuffer[Char]('H', 'i', 's') -> Array[String]("CAC", "CAU"), ArrayBuffer[Char]('G', 'l', 'n') -> Array[String]("CAA", "CAG"),
      ArrayBuffer[Char]('A', 's', 'n') -> Array[String]("AAC", "AAU"), ArrayBuffer[Char]('L', 'y', 's') -> Array[String]("AAA", "AAG"), ArrayBuffer[Char]('A', 's', 'p') -> Array[String]("GAC", "GAU"),
      ArrayBuffer[Char]('G', 'l', 'u') -> Array[String]("GAA", "GAG"), ArrayBuffer[Char]('C', 'y', 's') -> Array[String]("UGC", "UGU"),
      ArrayBuffer[Char]('U', 'r', 'p') -> Array[String]("UGG"),
      ArrayBuffer[Char]('A', 'r', 'g') -> Array[String]("CGA", "CGC", "CGG", "CGU", "AGA", "AGG"),
      ArrayBuffer[Char]('G', 'l', 'y') -> Array[String]("GGA", "GGC", "GGG", "GGU"))

  def is_complement(x: Char, y: Char): Boolean =
    if (((x == 'A') && (y == 'U')) || ((x == 'U') && (y == 'A')) || ((x == 'G') && (y == 'C')) || ((x == 'C') && (y == 'G'))) true else false

  def _matrix_v(str: String) = {
    length = str.length()
    var helping_list = ArrayBuffer[Double]()
    for (i <- 0 to str.length - 1) {
      for (j <- 0 to str.length - 1) {
        helping_list += 8.4
      }
      matrix_v += helping_list
      helping_list = ArrayBuffer[Double]()

    }
    for (i <- 0 to length - 1) {
      for (j <- 0 to length - 1) {
        if ((i > j - 4) && (i < j)) matrix_v(i)(j) = infinity
      }
    }
    for (i <- 0 to length - 1) {
      for (j <- 0 to length - 1) {
        if (i < j) {
          var my_list = ArrayBuffer[Double]()
          my_list += eh(i, j)
          my_list += es(i, j) + matrix_v(i + 1)(j - 1)
          my_list += VBI(i, j)
          var local_minimum = my_list(0)
          for (k <- 1 to my_list.length - 1) {
            if (local_minimum > my_list(k)) local_minimum = my_list(k)
          }
          matrix_v(i)(j) = local_minimum
        }
      }
    }
    zuker(str)
  }

  def one(i: Integer, j: Integer, str: java.lang.String): Double = {
    var flag: Boolean = false
    for (n <- i + 1 to j) {
      if (is_complement(str(i), str(n))) flag = true
    }
    if (flag) infinity
    else matrix_v(i + 1)(j)
  }

  def two(i: Integer, j: Integer, str: java.lang.String): Double = {
    var flag: Boolean = false
    for (n <- i + 0 to j - 1) {
      if (is_complement(str(j), str(n))) flag = true
    }
    if (flag) infinity
    else matrix_w(i)(j - 1)
  }

  def three(i: Integer, j: Integer, str: java.lang.String): Double = {
    if (!is_complement(str(i), str(j))) infinity
    else matrix_v(i)(j)
  }

  def four(i: Integer, j: Integer, str: java.lang.String): Double = {
    if (is_complement(str(i), str(j))) infinity
    var local_minimum: Double = matrix_w(i)(i + 1) + matrix_w(i + 2)(j)
    for (k <- i + 2 to j - 2) {
      if (matrix_w(i)(k) + matrix_w(k + 1)(j) < local_minimum) local_minimum = matrix_w(i)(k) + matrix_w(k + 1)(j)
    }
    local_minimum
  }

  def zuker(str: java.lang.String) = {
    var helping_list = ArrayBuffer[Double]()
    for (i <- 0 to length - 1) {
      for (j <- 0 to length - 1) {
        helping_list += 0
      }
      matrix_w += helping_list
      helping_list = ArrayBuffer[Double]()
    }
    for (i <- 0 to length - 1) {
      for (j <- 0 to length - 1) {
        if ((i > j - 4) && (i < j)) matrix_w(i)(j) = infinity
      }
    }
    for (i <- 0 to length - 1) {
      for (j <- 0 to length - 1) {
        if ((i < j) && (j - i > 4)) {
          var my_list = ArrayBuffer[Double]()
          my_list += one(i, j, str)
          my_list += two(i, j, str)
          my_list += three(i, j, str)
          my_list += four(i, j, str)
          var local_minimum = my_list(0)
          for (k <- 1 to 3) {
            if (local_minimum > my_list(k)) local_minimum = my_list(k)
          }
          matrix_w(i)(j) = local_minimum
        }
      }
    }
    for (i <- 0 to length - 1) {
      for (j <- 0 to length - 1) {
        if (matrix_w(i)(j) < infinity) sum += matrix_w(i)(j)
      }
    }
  }

  def eh(i: Integer, j: Integer): Double = {
    if ((length > 4) && (length < 10)) 4.4
    else if ((length > 9) && (length < 15)) 5.3
    else if ((length > 14) && (length < 20)) 5.8
    else if ((length > 19) && (length < 25)) 6.1
    else if ((length > 24) && (length < 30)) 6.3
    else if (length > 29) 6.5
    else infinity
  }

  def es(i: Integer, j: Integer): Double = {
    -0.5
  }

  def eb(i: Integer, j: Integer, a: Integer, b: Integer): Double = {
    -0.4
  }

  def VBI(i: Integer, j: Integer): Double = {
    var local_minimum: Double = infinity
    for (a <- i + 1 to j - 2) {
      for (b <- i + 2 to j - 1) {
        if ((a < b) && (a - i + j - b > 2)) {
          val d = eb(i, j, a, b) + matrix_v(a)(b)
          if (local_minimum > d) local_minimum = d
        }
      }
    }
    local_minimum
  }

  def monte_carlo(str: String, number: Integer): Double = {
    var optimal_structure: String = ""
    var optimal_energy: Double = 0
    var s = ArrayBuffer[Char]()
    length = str.length()
    for (i <- 0 to length - 1) {
      if ((i + 1) % 3 != 0) {
        s += str(i)
        print(s)
      }
      else if ((i + 1) % 3 == 0) {
        s += str(i)
        print(s)
        optimal_structure += d(s)(0)
        s = ArrayBuffer[Char]()
      }
    }
    _matrix_v(optimal_structure)
    optimal_energy = sum


    print(optimal_structure, optimal_energy)
    4.6
  }


  monte_carlo("PheSerProProAlaUrpUrpCysPhe", 20)


}
