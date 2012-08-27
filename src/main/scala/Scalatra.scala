import scala.reflect.macros.Context

trait Scalatra {
  import language.dynamics // piffle
  import language.experimental.macros // piffle

  def get(path: String)(action: => Any): Any = macro Macros.get

  val params = new Dynamic {
    def apply(propName: String): String = "foo"
    def selectDynamic(propName: String): String = macro Macros.selectDynamic
  }
}

object Macros { 
  def get(c: Context)(path: c.Expr[String])(action: c.Expr[Any]) = {
    import c.universe._

    val captureGroups = path.tree match {
      case Literal(Constant(p: String)) =>
        SinatraPathPatternParser(p).captureGroupNames
    }

    println(c.macroApplication)

    val routeParams = c.globalCache
      .getOrElse("routeParams", Vector.empty)
      .asInstanceOf[Seq[(c.Position, String)]]
    for ((pos, prop) <- routeParams if !captureGroups.contains(prop))
      c.error(pos, "Invalid route param name: "+prop)

    c.globalCache -= "routeParams"

    c.literalUnit
  }

  def selectDynamic(c: Context)(propName: c.Expr[String]) = {
    import c.universe._

    val routeParams = c.globalCache
      .getOrElse("routeParams", Vector.empty)
      .asInstanceOf[Seq[(c.Position, String)]]
    val p = propName.tree match {
      case Literal(Constant(p: String)) => p
    }
    c.globalCache("routeParams") = routeParams :+ (c.enclosingPosition -> p)

    // This is horrible.  TODO: learn DSL!
    c.Expr(c.parse("params(\"" + p + "\")"))
  }
}
