package cone.commandline

/**
 * @author Chris Turner
 */
private[commandline] sealed trait ArgumentElement
private[commandline] case class SimpleArgument(value: String) extends ArgumentElement
private[commandline] case class OptionArgument(name: String, value: Option[String]) extends ArgumentElement
private[commandline] case class FlagGroup(flags: Seq[FlagArgument]) extends ArgumentElement
private[commandline] case class FlagArgument(flag: Char) extends ArgumentElement

private[commandline] class ArgumentsTransformer {

  def transform(args: Array[String]): List[ArgumentElement] =
    args.toList.foldRight(List[ArgumentElement]())((arg, elements) => transformArg(arg) :: elements)

  private def transformArg(arg: String): ArgumentElement = arg match {
    case SimpleOption(name) => OptionArgument(name, None)
    case ParameterOption(name, value) => OptionArgument(name, asOption(value))
    case Flag(flag) => FlagArgument(flag)
    case CombinedFlags(flags) => FlagGroup(flags.map(FlagArgument(_)))
    case _ => SimpleArgument(arg)
  }

  private def asOption(s : String) = if ( s == "" ) None else Some(s)


  private object SimpleOption {
    private val Pattern = """^\-\-([a-zA-Z0-9\-_]+)$""".r
    def unapply(arg: String) = arg match {
      case Pattern(name) => Some(name)
      case _ => None
    }
  }

  private object ParameterOption {
    private val Pattern = """^\-\-([a-zA-Z0-9\-_]+)=(.*)$""".r
    def unapply(arg: String) = arg match {
      case Pattern(name, value) => Some((name, value))
      case _ => None
    }
  }

  private object Flag {
    private val Pattern = """^\-([a-zA-Z0-9])$""".r
    def unapply(arg: String) = arg match {
      case Pattern(flag) => Some(flag.head)
      case _ => None
    }
  }

  private object CombinedFlags {
    private val Pattern = """^\-([a-zA-Z0-9]{2,})$""".r
    def unapply(arg: String) = arg match {
      case Pattern(flags) => Some(flags.toList)
      case _ => None
    }
  }
}
