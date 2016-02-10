import scala.collection.mutable.ArrayBuffer

/**
  * Created by Alexander on 07.02.2016.
  */
class Main {
  var matrix_w = ArrayBuffer[ArrayBuffer[Double]]()
  var matrix_v = ArrayBuffer[ArrayBuffer[Double]]()
  var length = 0
  var sum: Double = 0
  var d: scala.collection.immutable.Map[java.lang.String, Array[java.lang.String]] =
    Map("Phe" -> Array[java.lang.String]("UUA", "UUC", "UUG", "UUU"), "Leu" -> Array[java.lang.String]("CUA", "CUC", "CUG", "CUU"),
      "Ile" -> Array[java.lang.String]("AUA", "AUC", "AUU"), "Met" -> Array[java.lang.String]("AUG"),
      "Ser" -> Array[java.lang.String]("UCA", "UCC", "UCG", "UCU", "AGU", "AGC"), "Pro" -> Array[java.lang.String]("CCA", "CCC", "CCG", "CCU"),
      "Uhr" -> Array[java.lang.String]("ACA", "ACC", "ACG", "ACU"), "Ala" -> Array[java.lang.String]("GCA", "GCC", "GCG", "GCU"),
      "Uyr" -> Array[java.lang.String]("UAC", "UAU"), "His" -> Array[java.lang.String]("CAC", "CAU"), "Gln" -> Array[java.lang.String]("CAA", "CAG"),
      "Asn" -> Array[java.lang.String]("AAC", "AAU"), "Lys" -> Array[java.lang.String]("AAA", "AAG"), "Asp" -> Array[java.lang.String]("GAC", "GAU"),
      "Glu" -> Array[java.lang.String]("GAA", "GAG"), "Cys" -> Array[java.lang.String]("UGC", "UGU"), "Urp" -> Array[java.lang.String]("UGG"),
      "Arg" -> Array[java.lang.String]("CGA", "CGC", "CGG", "CGU", "AGA", "AGG"),
      "Gly" -> Array[java.lang.String]("GGA", "GGC", "GGG", "GGU"))

  def is_complement(x: Char, y: Char): Boolean =
    if (((x == 'A') && (y == 'U')) || ((x == 'U') && (y == 'A')) || ((x == 'G') && (y == 'C')) || ((x == 'C') && (y == 'G'))) true else false


  def _matrix_v(str: java.lang.String) = {
    val a = new Main()
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
    a.zuker(str)
  }

  def eh(i: Integer, j: Integer): Double = {
    val b = new Main()
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
    val c = new Main()
    var local_minimum: Double = 10000000
    for (a <- i + 1 to j - 2) {
      for (b <- i + 2 to j - 1) {
        if ((a < b) && (a - i + j - b > 2)) {
          val d = c.eb(i, j, a, b) + c.matrix_v(a)(b)
          if (local_minimum > d) local_minimum = d
        }
      }
    }
    local_minimum
  }

  def one(i: Integer, j: Integer, str: java.lang.String): Double = {
    val a = new Main()
    var flag: Boolean = false
    for (n <- i + 1 to j) {
      if (a.is_complement(str(i), str(j))) flag = true
    }
    if (flag) 10000000
    else a.matrix_w(i + 1)(j)
  }

  def two(i: Integer, j: Integer, str: java.lang.String): Double = {
    val a = new Main()
    var flag: Boolean = false
    for (n <- i + 0 to j - 1) {
      if (a.is_complement(str(j), str(n))) flag = true
    }
    if (flag) 10000000
    else a.matrix_w(i)(j - 1)
  }

  def three(i: Integer, j: Integer, str: java.lang.String): Double = {
    val a = new Main()
    if (a.is_complement(str(i), str(j))) 10000000
    var local_minimum: Double = a.matrix_w(i)(i + 1) + a.matrix_w(i + 2)(j)
    for (k <- i + 2 to j - 2) {
      if (a.matrix_w(i)(k) + a.matrix_w(k + 1)(j) < local_minimum) local_minimum = a.matrix_w(i)(k) + a.matrix_w(k + 1)(j)
    }
    local_minimum
  }

  def four(i: Integer, j: Integer, str: java.lang.String): Double = {
    val a = new Main()
    if (!a.is_complement(str(i), str(j))) 10000000
    else a.matrix_v(i)(j)
  }

  def zuker(str: java.lang.String) = {
    val a = new Main()
    var helping_list = ArrayBuffer[Double]()
    for (i <- 0 to a.length - 1) {
      for (j <- 0 to a.length - 1) {
        helping_list += 0
      }
      a.matrix_w += helping_list
      helping_list = ArrayBuffer[Double]()
    }
    for (i <- 0 to a.length - 1) {
      for (j <- 0 to a.length - 1) {
        if ((i > j - 4) && (i < j)) a.matrix_w(i)(j) = 10000000
      }
    }
    for (i <- 0 to a.length - 1) {
      for (j <- 0 to a.length - 1) {
        if ((i < j) && (j - i > 4)) {
          var my_list = ArrayBuffer[Double]()
          my_list += a.one(i, j, str)
          my_list += a.two(i, j, str)
          my_list += a.three(i, j, str)
          my_list += a.four(i, j, str)
          var local_minimum = my_list(0)
          for (k <- 1 to my_list.length - 1) {
            if (local_minimum > my_list(k)) local_minimum = my_list(k)
          }
          a.matrix_w(i)(j) = local_minimum
        }
      }
    }
    for (i <- 0 to a.length - 1) {
      for (j <- 0 to a.length - 1) {
        if (a.matrix_w(i)(j) < 10000000) a.sum = a.matrix_w(i)(j)
      }
    }
  }

  def count(str: java.lang.String, number: Integer): Double = {
    val a = new Main()
    var optimal_structure: java.lang.String = ""
    var s: java.lang.String = ""
    for (i <- 0 to a.length - 1) {
      if (i % 3 != 0) {
        s += str(i)
      }
      else if (i % 3 == 0) {
        s += str(i)
        val t: Array[java.lang.String] = a.d(s)
        optimal_structure += t(0)
        s = ""
      }
    }
    s = ""
    a._matrix_v(optimal_structure)
    var optimal_energy = a.sum
    for (k <- 1 to number) {
      var this_structure = ""
      for (i <- 0 to a.length - 1) {
        if (i % 3 != 0) {
          s += str(i)
        }
        else if (i % 3 == 0) {
          s += str(i)
          val t: Array[java.lang.String] = a.d(s)
          val j = t.length - 1 //Здесь можно выбирать случайное число из отрезка [0, length - 1]
          this_structure += t(j)
          s = ""
        }
      }
      a._matrix_v(this_structure)
      if (a.sum < optimal_energy) {
        optimal_energy = a.sum
        optimal_structure = this_structure
      }
    }
    print(optimal_structure)
    optimal_energy
  }

  def main(args: Array[java.lang.String]): Unit = {
    val k = new Main()
    print(k.count("ProGlu", 20))
  }
}
