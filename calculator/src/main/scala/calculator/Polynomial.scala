package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b()*b()-4*a()*c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      computeDelta(a,b,c)() match {
        case d if d<0 => Set()
        case d if d==0 => Set(-(b()/2*a()))
        case d => {
          val x1 = (-b()+math.sqrt(d))/(2*a())
          val x2 = (-b()-math.sqrt(d))/(2*a())
          Set(x1,x2)
        }
      }
    }
  }
}
