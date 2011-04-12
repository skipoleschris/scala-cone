package cone.arguments

/**
 * @author Chris Turner
 */
trait Argument {

  def ++(parameters: List[Argument]): Argument = add(parameters)

  def isDuplicateOf(arg: Argument): Boolean

  protected def add(parameters: List[Argument]): Argument = sys.error("Adding child arguments as parameters is not supported")
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

case class FlagArgument(flag: Char, parameters: List[Argument] = List()) extends Argument {


  def isDuplicateOf(arg: Argument) = arg match {
    case FlagArgument(f, _) => f == flag
    case _ => false
  }

  override def add(params: List[Argument]) = copy(parameters = parameters ++ params)
}
