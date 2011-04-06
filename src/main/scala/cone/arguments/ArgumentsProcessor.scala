package cone.arguments

/**
 * @author Chris Turner
 */
class ArgumentsProcessor(specification: ArgumentSpecification) {

  private lazy val BooleanOptionPattern = """^\-\-([a-zA-Z0-9_\-]+)$""".r
  private lazy val ParameterOptionPattern = """^\-\-([a-zA-Z0-9_\-]+)(=(.*))?$""".r
  private lazy val FlagPattern = """^\-([a-zA-Z0-9]+)$""".r


  def processArguments(args: Array[String]) = {
    val acc = args.toList.foldLeft(ArgumentAccumulator.create)(processArgument)
    if ( acc.isExpecting ) acc + InsufficientFlagParameters(acc.argumentWithExpectations.get)
    else acc
  }

  private def processArgument(acc: ArgumentAccumulator, arg: String) =
    if ( acc.isExpecting ) processExpectation(acc, arg)
    else processNewArgument(acc, arg)

  private def processNewArgument(acc: ArgumentAccumulator, arg: String) = arg match {
    case BooleanOptionPattern(name) => applyArgument(OptionArgument.create(name, null), acc)
    case ParameterOptionPattern(name, _, value) => applyArgument(OptionArgument.create(name, value), acc)
    case FlagPattern(flags) => flags.foldRight(acc)(processFlag)
    case _ => applyArgument(SimpleArgument(arg), acc)
  }

  private def processExpectation(acc: ArgumentAccumulator, arg: String) = acc.expectationsRemaining match {
    case Nil => sys.error("Unexpected expectation argument processed when none was expected")
    case x::xs => {
      x(SimpleArgument(arg)) match {
        case Valid(param) => acc + param
        case Error(cause) => acc + InvalidFlagParameter(acc.argumentWithExpectations.get) + cause
      }
    }
  }

  private def processFlag(c: Char, acc: ArgumentAccumulator) = applyArgument(FlagArgument(c), acc)

  private def applyArgument(argument: Argument, acc: ArgumentAccumulator) =
    specification.applyRuleFor(argument, acc.noOfSimpleArguments) match {
      case Valid(arg) => acc + arg
      case Expectation(flag, rules) => acc.expecting(flag, rules)
      case Error(cause) => acc + cause
    }
}
