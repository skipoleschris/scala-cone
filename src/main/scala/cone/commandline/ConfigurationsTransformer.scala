package cone.commandline

import annotation.tailrec

/**
 * @author Chris Turner
 */
private[commandline] class ConfigurationsTransformer(specification: ConfigurationSpec) {

  def processConfigurations(arguments: List[ArgumentElement]) =
    processNextOption(Accumulator(ProcessedConfigurations(), arguments))

  @tailrec
  private def processNextOption(acc: Accumulator): ProcessedConfigurations = acc.remainder match {
    case Nil => acc.contents
    case OptionArgument(name, value) :: xs => processNextOption(processOption(name, value, acc))
    case FlagGroup(flags) :: xs => processNextOption(flags.foldLeft(acc.next)((a, f) => processFlag(f.flag, a)))
    case FlagArgument(flag) :: xs => processNextOption(processFlag(flag, acc.next))
    case SimpleArgument(value) :: xs => processNextOption(acc.consumeValue(value))
  }

  private def processOption(option: String, value: Option[String], acc: Accumulator) = specification.ruleForOption(option) match {
    case None => acc.addError(UnknownConfigurationOption(option))
    case Some(rule) => rule.applyOption(value) match {
      case configuration: Configuration => acc.consumeOption(configuration)
      case error: ConfigurationError => acc.addError(error)
    }
  }

  private def processFlag(flag: Char, acc: Accumulator) = specification.ruleForFlag(flag) match {
    case None => acc.addError(UnknownConfigurationOption("" + flag))
    case Some(rule) => rule.applyFlag(acc.remainder) match {
      case c: Configuration if ( c.value == None ) => acc.consumeOptionLeavingRemainder(c)
      case c: Configuration => acc.consumeOption(c)
      case error: ConfigurationError => acc.addError(error)
    }
  }

  private case class Accumulator(contents: ProcessedConfigurations,
                                 remainder: List[ArgumentElement]) {
    def consumeOption(newOption: Configuration) = copy(contents + newOption, remainder.tail)
    def consumeOptionLeavingRemainder(newOption: Configuration) = copy(contents + newOption, remainder)
    def consumeValue(value: String) = copy(contents + value, remainder.tail)
    def next = copy(contents, remainder.tail)
    def addError(error: ConfigurationError) = copy(contents + error, remainder.tail)
  }
}
