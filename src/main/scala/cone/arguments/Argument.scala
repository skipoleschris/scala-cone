package cone.arguments

/**
 * @author Chris Turner
 */
trait Argument {

  def +(arg: Argument): Argument = add(arg)

  protected def add(arg: Argument): Argument = sys.error("Adding child arguments as parameters is not supported")
}

case class SimpleArgument(value: String) extends Argument

case class OptionArgument(name: String, value: Option[String]) extends Argument

object OptionArgument {
  def create(name: String, value: String) = OptionArgument(name, Option(value))
}

case class FlagArgument(flag: Char, parameters: List[Argument] = List()) extends Argument {

  override def add(arg: Argument) = copy(parameters = arg :: parameters)
}
