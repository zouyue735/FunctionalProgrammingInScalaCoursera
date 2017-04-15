package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal({
      val _b = b()
      val _a = a()
      val _c = c()
      _b * _b - 4 * _a * _c
    })
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val _delta = delta()
      if (_delta < 0) Set()
      else {
        val _a = a()
        val _b = b()
        Set((-_b + Math.sqrt(_delta)) / 2 / _a, (-_b - Math.sqrt(_delta)) / 2 / _a)
      }
    }
  }
}
