trait CarElement {
  def accept(visitor: CarElementVisitor): Unit = {
    visitor visit this
  }
}

trait CarElementVisitor {
  // def visit(body: Body): Unit
  // def visit(car: Car): Unit
  // def visit(eingine: Engine): Unit
  // def visit(wheel: Wheel): Unit
  def visit(elem: CarElement): Unit
}

case class Body() extends CarElement {

}

case class Car(val elements: Seq[CarElement]) extends CarElement {
  override def accept(visitor: CarElementVisitor): Unit = {
    elements foreach {_ accept visitor}
    visitor visit this
  }
}

case class Engine() extends CarElement {

}

case class Wheel(name: String) extends CarElement {

}

case class CarElementDoVisitor() extends CarElementVisitor {
  // override def visit(b: Body) {
  //   println("Moving mody.")
  // }
  // override def visit(c: Car) {
  //   println("Starting car.")
  // }
  // override def visit(w: Wheel) {
  //   println(s"Kicking weel ${w.name}.")
  // }
  // override def visit(e: Engine) {
  //   println("Staring engine.")
  // }
  override def visit(elem: CarElement): Unit = {
    elem match {
      case (b: Body)   => println("Moving body.")
      case (c: Car)    => println("Starting car.")
      case (w: Wheel)  => println(s"Kicking the whell ${w.name}.")
      case (e: Engine) => println("Starting engine.")
    }
  }
}
