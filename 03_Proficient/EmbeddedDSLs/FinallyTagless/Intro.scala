abstract class Exp {
  def eval: Int
}

case class Lit(n: Int) extends Exp {
  override def eval = n
}

case class Add(x: Exp, y: Exp) extends Exp {
  override def eval = x.eval + y.eval
}

case class Mul(x: Exp, y: Exp) extends Exp {
  override def eval = x.eval * y.eval
}

println(Lit(3).eval)
