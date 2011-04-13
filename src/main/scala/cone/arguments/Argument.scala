package cone.arguments

/**
 * @author Chris Turner
 */
sealed trait Argument {
  def isDuplicateOf(arg: Argument): Boolean
}

trait Parameterised {
  def parameters: List[Argument]
  def +(parameter: Argument): Argument = add(parameter)
  def ++(parameters: List[Argument]): Argument = add(parameters)

  protected def add(parameter: Argument): Argument
  protected def add(parameters: List[Argument]): Argument
}

case class SimpleArgument(value: String) extends Argument {
  def isDuplicateOf(arg: Argument) = false
}

case class OptionArgument(name: String, value: Option[String]) extends Argument {
  def isDuplicateOf(arg: Argument) = arg match {
    case OptionArgument(n, _) => n == name
    case _ => false
  }
}

object OptionArgument {
  def create(name: String, value: String) = OptionArgument(name, Option(value))
}

case class FlagArgument(flag: Char,
                        parameters: List[Argument] = List()) extends Argument with Parameterised {
  def isDuplicateOf(arg: Argument) = arg match {
    case FlagArgument(f, _) => f == flag
    case _ => false
  }

  protected def add(p: Argument) = copy(parameters = p :: parameters)
  protected def add(p: List[Argument]) = copy(parameters = p ++ parameters)
}
