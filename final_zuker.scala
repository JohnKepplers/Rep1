import scala.collection.mutable.ArrayBuffer
import java.util.Random

object Main extends App {
  val infinity = 99999
  var matrix_w = ArrayBuffer[ArrayBuffer[Double]]()
  var matrix_v = ArrayBuffer[ArrayBuffer[Double]]()
  var length = 0
  var sum: Double = 0
  var matrix_of_stacked_pairs = ArrayBuffer[ArrayBuffer[Double]](ArrayBuffer[Double](-0.9, -1.8, -2.3, -1.1), ArrayBuffer[Double](-1.7 - 2.9 - 3.4 - 2.3),
    ArrayBuffer[Double](-2.1 - 2.0 - 2.9 - 1.8), ArrayBuffer[Double](-0.9 - 1.7 - 2.1 - 0.9))
  val d: scala.collection.immutable.Map[ArrayBuffer[Char], Array[String]] =
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
          my_list += eh(i, j, str)
          my_list += es(i, j, str) + matrix_v(i + 1)(j - 1)
          my_list += VBI(i, j, str)
          var local_minimum = my_list(0)
          for (k <- 1 to 2) {
            if (local_minimum > my_list(k)) local_minimum = my_list(k)
          }
          matrix_v(i)(j) = local_minimum
        }
      }
    }
    zuker(str)
  }

  def one(i: Integer, j: Integer, str: String): Double = {
    var flag: Boolean = false
    for (n <- i + 1 to j) {
      if (is_complement(str(i), str(n))) flag = true
    }
    if (flag) infinity
    else matrix_v(i + 1)(j)
  }

  def two(i: Integer, j: Integer, str: String): Double = {
    var flag: Boolean = false
    for (n <- i + 0 to j - 1) {
      if (is_complement(str(j), str(n))) flag = true
    }
    if (flag) infinity
    else matrix_w(i)(j - 1)
  }

  def three(i: Integer, j: Integer, str: String): Double = {
    if (!is_complement(str(i), str(j))) infinity
    else matrix_v(i)(j)
  }

  def four(i: Integer, j: Integer, str: String): Double = {
    if (is_complement(str(i), str(j))) infinity
    var local_minimum: Double = matrix_w(i)(i + 1) + matrix_w(i + 2)(j)
    for (k <- i + 2 to j - 2) {
      if (matrix_w(i)(k) + matrix_w(k + 1)(j) < local_minimum) local_minimum = matrix_w(i)(k) + matrix_w(k + 1)(j)
    }
    local_minimum
  }

  def zuker(str: String) = {
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

  def is_hairpin_loop(i: Integer, j: Integer, str: String): Boolean = {
    var flag: Boolean = true
    if (((j - i) > 0) && ((j - i) % 2 == 0)) {
      for (k <- 0 to (j - i) / 2 - 1) {
        if (is_complement(str(i + 1 + k), str(j - k - 1))) {
          flag = false
        }
      }
    }
    if (((j - i) > 0) && ((j - i) % 2 == 1)) {
      for (k <- 0 to (j - i - 1) / 2) {
        if (is_complement(str(i + 1 + k), str(j - k - 1))) {
          flag = false
        }
      }
    }
    if (flag) true else false
  }

  def is_stacking_loop(i: Integer, j: Integer, str: String): Boolean = {
    if ((is_complement(str(i + 1), str(j - 1))) && (is_hairpin_loop(i + 1, j - 1, str))) true else false
  }

  def is_interior_loop(i1: Integer, j1: Integer, i2: Integer, j2: Integer, str: String): Boolean = {
    if ((i2 != i1 + 1) && (j2 != j1 - 1) && (is_complement(str(i2), str(j2)))) true else false
  }


  def eh(i: Integer, j: Integer, str: String): Double = {
    // (i, j) All unpaired bases between i & j, if there is no paired bases between i & j
    if (!is_hairpin_loop(i, j, str)) infinity
    else {
      val length_of_loop = Math.abs(str(j) - str(i)) - 1
      if (length_of_loop == 3) 4.1
      else if (length_of_loop == 4) 4.9
      else if (length_of_loop == 5) 4.4
      else if ((length_of_loop >= 5) && (length_of_loop < 10)) 5.3
      else if ((length_of_loop >= 10) && (length_of_loop < 15)) 5.8
      else if ((length_of_loop >= 15) && (length_of_loop < 20)) 6.1
      else if ((length_of_loop >= 20) && (length_of_loop < 25)) 6.3
      else if ((length_of_loop >= 25) && (length_of_loop < 31)) 6.5
      else infinity
    }
  }

  def es(i: Integer, j: Integer, str: String): Double = {
    // T = 310K
    if (!is_stacking_loop(i, j, str)) infinity
    else {
      if ((i == 'A') && (j == 'U') && (i + 1 == 'A') && (j - 1 == 'U')) matrix_of_stacked_pairs(0)(0)
      if ((i == 'A') && (j == 'U') && (i + 1 == 'C') && (j - 1 == 'G')) matrix_of_stacked_pairs(0)(1)
      if ((i == 'A') && (j == 'U') && (i + 1 == 'G') && (j - 1 == 'C')) matrix_of_stacked_pairs(0)(2)
      if ((i == 'A') && (j == 'U') && (i + 1 == 'U') && (j - 1 == 'A')) matrix_of_stacked_pairs(0)(3)
      if ((i == 'C') && (j == 'G') && (i + 1 == 'A') && (j - 1 == 'U')) matrix_of_stacked_pairs(1)(0)
      if ((i == 'C') && (j == 'G') && (i + 1 == 'C') && (j - 1 == 'G')) matrix_of_stacked_pairs(1)(1)
      if ((i == 'C') && (j == 'G') && (i + 1 == 'G') && (j - 1 == 'C')) matrix_of_stacked_pairs(1)(2)
      if ((i == 'C') && (j == 'G') && (i + 1 == 'U') && (j - 1 == 'A')) matrix_of_stacked_pairs(1)(3)
      if ((i == 'G') && (j == 'C') && (i + 1 == 'A') && (j - 1 == 'U')) matrix_of_stacked_pairs(2)(0)
      if ((i == 'G') && (j == 'C') && (i + 1 == 'C') && (j - 1 == 'G')) matrix_of_stacked_pairs(2)(1)
      if ((i == 'G') && (j == 'C') && (i + 1 == 'G') && (j - 1 == 'C')) matrix_of_stacked_pairs(2)(2)
      if ((i == 'G') && (j == 'C') && (i + 1 == 'U') && (j - 1 == 'A')) matrix_of_stacked_pairs(2)(3)
      if ((i == 'U') && (j == 'A') && (i + 1 == 'A') && (j - 1 == 'U')) matrix_of_stacked_pairs(3)(0)
      if ((i == 'U') && (j == 'A') && (i + 1 == 'C') && (j - 1 == 'G')) matrix_of_stacked_pairs(3)(1)
      if ((i == 'U') && (j == 'A') && (i + 1 == 'G') && (j - 1 == 'C')) matrix_of_stacked_pairs(3)(2)
      if ((i == 'U') && (j == 'A') && (i + 1 == 'U') && (j - 1 == 'A')) matrix_of_stacked_pairs(3)(3)
      else infinity
    }
  }

  def ebi(i: Integer, j: Integer, a: Integer, b: Integer, str: String): Double = {
    if (!is_interior_loop(i, j, a, b, str)) infinity
    else {
      val length_of_loop = Math.abs(str(j) - str(i)) - 1
      if (length_of_loop == 2) 4.1
      else if (length_of_loop == 3) 5.1
      else if (length_of_loop == 4) 4.9
      else if (length_of_loop == 5) 5.3
      else if ((length_of_loop >= 5) && (length_of_loop < 10)) 6.3
      else if ((length_of_loop >= 10) && (length_of_loop < 15)) 6.7
      else if ((length_of_loop >= 15) && (length_of_loop < 20)) 7.0
      else if ((length_of_loop >= 20) && (length_of_loop < 25)) 7.2
      else if ((length_of_loop >= 25) && (length_of_loop < 31)) 7.4
      else infinity
    }
  }

  def VBI(i: Integer, j: Integer, str: String): Double = {
    var local_minimum: Double = infinity
    for (a <- i + 1 to j - 2) {
      for (b <- i + 2 to j - 1) {
        if ((a < b) && (a - i + j - b > 2)) {
          val d = ebi(i, j, a, b, str) + matrix_v(a)(b)
          if (local_minimum > d) local_minimum = d
        }
      }
    }
    local_minimum
  }

  def monte_carlo(str: String, number: Integer): Integer = {
    var optimal_structure: String = ""
    var optimal_energy: Double = 0
    var s = ArrayBuffer[Char]()
    length = str.length()
    for (i <- 0 to length - 1) {
      if ((i + 1) % 3 != 0) {
        s += str(i)
      }
      else if ((i + 1) % 3 == 0) {
        s += str(i)
        optimal_structure += d(s)(0)
        s = ArrayBuffer[Char]()
      }
    }
    _matrix_v(optimal_structure)
    optimal_energy = sum
    for (i <- 2 to number) {
      var this_structure: String = ""
      var this_s = ArrayBuffer[Char]()
      for (j <- 0 to length - 1) {
        if ((j + 1) % 3 != 0) {
          this_s += str(j)
        }
        else if ((j + 1) % 3 == 0) {
          this_s += str(j)
          val rand = new Random()
          val r = rand.nextInt(d(this_s).length)
          this_structure += d(this_s)(r)
          this_s = ArrayBuffer[Char]()
        }
        _matrix_v(this_structure)
        if (sum < optimal_energy) {
          optimal_structure = this_structure
          optimal_energy = sum
        }
      }
    }
    print(optimal_structure, optimal_energy)
    1
  }


  monte_carlo("PheSerProProAlaUrpUrpCysPhe", 10)


}
