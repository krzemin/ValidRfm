import scala.xml._

object RfmParser extends App {

  case class Generation(hex: String, variants: Seq[Variant])
  case class Variant(hex: String, revisions: Seq[Revision])
  case class Revision(hex: String, support: Boolean)

  def parseXml(elem: Elem): Seq[Generation] = (elem \ "Generation").map { generation =>
       Generation(generation \@ "hex", (generation \ "Variant").map { variant =>
         Variant(variant \@ "hex", (variant \ "Revision").map { revision =>
           Revision(revision \@ "hex", (revision \ "Official_support" \@ "value") == "yes")
         })
       })
    }

  trait AstVar
  case object Gen extends AstVar { override def toString = "Gen" }
  case object Var extends AstVar { override def toString = "Var" }
  case object E extends AstVar { override def toString = "E" }

  trait Expr
  case class Eq(x: AstVar, y: String) extends Expr { override def toString = s"$x $y eq" }
  case class Ge(x: AstVar, y: String) extends Expr { override def toString = s"$x $y ge" }
  case class Le(x: AstVar, y: String) extends Expr { override def toString = s"$x $y le" }
  case class And(x: Expr, y: Expr) extends Expr { override def toString = s"$x $y and" }
  case class Or(x: Expr, y: Expr) extends Expr { override def toString = s"$x $y or" }
  case object False extends Expr { override def toString = s"0" }

  def optimizeExpr(e: Expr): Expr = e match {
    case Or(False, x) => optimizeExpr(x)
    case Or(x, False) => optimizeExpr(x)
    case Or(x, y) => Or(optimizeExpr(x), optimizeExpr(y))
    case And(x, y) => And(optimizeExpr(x), optimizeExpr(y))
    case _ => e
  }

  def genVariant(variant: Variant): Expr =
    variant.revisions.filter(_.support).sortBy(_.hex) match {
      case Seq() => False
      case Seq(rev) => And(Eq(Var, variant.hex), Eq(E, rev.hex))
      case revs => And(Eq(Var, variant.hex), And(Ge(E, revs.head.hex), Le(E, revs.last.hex)))
    }

  def genGeneration(generation: Generation): Expr = And(
    Eq(Gen, generation.hex),
    generation.variants.map(variant => genVariant(variant)).reduce((x, y) => Or(x, y))
  )

  def genExpr(generations: Seq[Generation]): Expr = generations
      .map(generation => genGeneration(generation))
      .reduce((x,y) => Or(x, y))

  def parse(elem: Elem): String = {
    val generations = parseXml(elem)
    val expr = genExpr(generations)
    val optimizedExpr = optimizeExpr(expr)
    optimizedExpr.toString
  }
}
