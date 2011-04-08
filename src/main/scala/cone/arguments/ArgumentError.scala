package cone.arguments

/**
 * @author Chris Turner
 */
trait ArgumentError

case class UnknownOption(option: OptionArgument) extends ArgumentError
case class OptionValueNotSupplied(option: OptionArgument) extends ArgumentError
case class OptionValueNotSupported(option: OptionArgument) extends ArgumentError
case class OptionValueInvalid(option: OptionArgument) extends ArgumentError

case class UnknownFlag(flag: FlagArgument) extends ArgumentError

case class DuplicateArgument(arg: Argument) extends ArgumentError
case class InvalidFlagParameter(flag: Argument) extends ArgumentError
case class InsufficientFlagParameters(flag: Argument) extends ArgumentError
case class MultipleParameterExpectations(arg: Argument) extends ArgumentError

case class NonMatchingArgumentPattern(simple: SimpleArgument, expected: String) extends ArgumentError
case class TooManyArguments(simple: SimpleArgument) extends ArgumentError
case class InsufficientArguments(expected: Int, actual: Int) extends ArgumentError
