/** Object Algebras **/

/** abstract factory */
trait ExpAlg[T] {
  def lit(n: Int): T
  def add(x: T, y: T): T
}

/* e1 = (1 + (2 + 3)) */
def e1[T](f: ExpAlg[T]): T = {
  f.add(
    f.lit(1),
    f.add(
      f.lit(2),
      f.lit(3)))
}


/** concrete factory
  + any object extending ExpAlg
  + different ways for interpretation
  */

case class ExpAlgInt() extends ExpAlg[Int] {
  type T = Int
  def lit(n: Int): T = { n }
  def add(x: T, y: T): T = { x + y}
}

case class ExpAlgString() extends ExpAlg[String] {
  type T = String
  def lit(n: Int): T = { s"$n" }
  def add(x: T, y: T): T = { s"($x + $y)"  }
}



trait Eval {
  def eval: Int
}

class EvalExp extends ExpAlg[Eval] {
  type T = Eval
  def lit(n: Int): T = new T {
    def eval = n
  }
  def add(x: T, y: T): T = new T {
    def eval = x.eval + y.eval
  }
}


val v1 = e1(new EvalExp).eval
println(v1)


/* When needing new method [mul] ... */

trait MulAlg[T] extends ExpAlg[T] {
  def mul(x: T, y: T): T
}


def e2[T](f: MulAlg[T]): T = {
  f.mul(
    f.lit(4),
    f.add(
      f.lit(5),
      f.lit(6)))
}

class EvalMul extends
    EvalExp with
    MulAlg[Eval] {
  def mul(x: T, y: T): T = new T {
    def eval = x.eval * y.eval
  }
}

val v2 = e2(new EvalMul).eval
println(v2)


/* Further interface with methods */

trait View {
  def view: String
}

class ViewExp extends ExpAlg[View] {
  type T = View
  def lit(n: Int): T = new View {
    def view = s"$n"
  }
  def add(x: T, y: T): T = new View {
    def view = s"(${x.view} + ${y.view})"
  }
}
class ViewMul extends ViewExp with MulAlg[View] {
  def mul(x: T, y: T): T = new View {
    def view = s"(${x.view} * ${y.view})"
  }
}


println(e1(new ViewExp).view)
println(e2(new ViewMul).view)


/** 
  * object algebra interface
  * [ExpAlg[T]]
  * 
  * object algebras
  * - concrete algebra
  */

