package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b()*b()-4*a()*c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal(delta() match {
      case d if d < 0 => Set()
      case d if d == 0 => Set(-b()/(2*a()))
      case d => Set(1, -1).map(s => (-b()+s*math.sqrt(d))/(2*a()))
    })
  }
}
