object Program {

  case class Matrix(columns: Int, rows: Int, values: Array[Array[Double]]) {
    def at(column: Int, row: Int): Double = values(row)(column)
  }

  object Matrix {
    def apply(columns: Int, rows: Int)(f: (Int, Int) => Double) =
      new Matrix(columns, rows, Array.tabulate(rows, columns) { (r, c) => f(c, r) })

    def multiply(a: Matrix, b: Matrix): Matrix = {
      if (a.columns != b.rows) sys.error("a.columns must be equal to b.rows")

      val columns = b.columns
      val rows = a.rows

      Matrix(columns, rows) { (c, r) =>
        (0 until a.columns).foldLeft(0.0) { (s, i) => s + a.at(i, r) * b.at(c, i) }
      }
    }

    // Oracle implementation of multiply. From:
    //  https://rosettacode.org/wiki/Matrix_multiplication#Scala
    def multiplyOracle[A](a: Matrix, b: Matrix): Matrix = {
      if (a.columns != b.rows) sys.error("a.columns must be equal to b.rows")
      val values = for (row <- a.values)
        yield for (col <- b.values.transpose)
          yield row zip col map Function.tupled(_ * _) reduceLeft (_ + _)
      Matrix(b.columns, a.columns, values)
    }
  }

  def timeIt(repeat: Int)(action: => Unit): Double = {
    val before = System.nanoTime
    for (_ <- 0 until repeat) {
      action
    }
    val after = System.nanoTime
    ((after - before) / 1E6).round
  }

  def randomMatrix(columns: Int, rows: Int): Matrix = {
    Matrix(columns, rows) { (_, _) => math.random }
  }

  def main(argv: Array[String]): Unit = {
    // Compare oracle and our implementation
    {
      val a = randomMatrix(100, 100)
      val b = randomMatrix(100, 100)
      val expected = Matrix.multiplyOracle(a, b)
      val actual = Matrix.multiply(a, b)
      if (expected.columns == actual.columns && expected.rows == actual.rows) {
        val evs = expected.values.flatten
        val avs = actual.values.flatten

        if (evs.length != avs.length) {
          sys.error("Oracle and our implementation generates different amount of values")
        }

        val diff = evs
          .zip(avs)
          .map((x: (Double, Double)) => x._1 - x._2)
          .map(x => x * x)
          .sum / (expected.columns * actual.columns + 1)

        if (diff < 0.000001) {
          println("Oracle and our implementation are in agreement for a random set of matrices")
        } else {
          sys.error(s"Too large diff between Oracle and our implementation: $diff")
        }
      } else {
        sys.error("Dimensions must match")
      }
    }

    // Three iterations to warm-up the code
    val totals = List(100000000, 100000000, 100000000)

    for (total <- totals) {
      val dimensions = List(
        (10, 10, 10),
        (100, 100, 100),
        (1000, 100, 1000)
//        (1000, 1000, 1000)
      )
      for ((x, y, z) <- dimensions) {
        val a = randomMatrix(x, y)
        val b = randomMatrix(z, x)

        val repeat = total / (x * y * z)
        if (repeat > 0) {
          println(s"Repeating $repeat times with dimensions ($x, $y, $z)")
          val ms = timeIt(repeat) {
            Matrix.multiply(a, b)
          }
          println(s"  It took $ms milliseconds")
        }
      }
    }
  }
}
