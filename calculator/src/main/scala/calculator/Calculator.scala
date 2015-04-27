package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
        namedExpressions.map { case (name, expr) =>
          (name, Signal(eval(expr(), namedExpressions)))
        }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(v) => v
      case Ref(name) => eval(getReferenceExpr(name, references), references)
      case Plus(a, b) => eval(a, references) + eval(b, references)
      case Minus(a, b) => eval(a, references) - eval(b, references)
      case Times(a, b) => eval(a, references) * eval(b, references)
      case Divide(a, b) => eval(a, references) / eval(b, references)
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal() match {
        case expr if isCyclic(expr, references, Set(name)) => Literal(Double.NaN)
        case expr => expr
      }
    }
  }

  private def isCyclic(expr: Expr, references: Map[String, Signal[Expr]], names: Set[String]): Boolean = {
    expr match {
      case Literal(v) => false
      case Ref(name) if names(name) => true
      case Ref(name) => isCyclic(getReferenceExpr(name, references), references, names + name)
      case Plus(a, b) => isCyclic(a, references, names) || isCyclic(b, references, names)
      case Minus(a, b) => isCyclic(a, references, names) || isCyclic(b, references, names)
      case Times(a, b) => isCyclic(a, references, names) || isCyclic(b, references, names)
      case Divide(a, b) => isCyclic(a, references, names) || isCyclic(b, references, names)
    }
  }
}
