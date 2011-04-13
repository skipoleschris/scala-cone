package cone.arguments

/**
 * @author Chris Turner
 */
sealed trait ArgumentRule

private[arguments] trait ValuePatternRule {
  def valuePattern: String
  protected lazy val pattern = valuePattern.r
}

private[arguments] object ValuePatternRule {
  def DefaultValuePattern = ".+"
}

import ValuePatternRule._

private[arguments] trait ParameterisedRule {
  def parameterSpecification: Option[ArgumentSpecification]
}

case class OptionRule(name: String,
                      valueRequired: Boolean = false,
                      valuePattern: String = DefaultValuePattern) extends ArgumentRule with ValuePatternRule {
   def apply(option: OptionArgument) =
    if ( option.value == None ) checkBooleanOption(option) else checkValueOption(option)

  private def checkBooleanOption(option: OptionArgument) =
    if ( valueRequired ) Error(OptionValueNotSupplied(option)) else Valid(option)

  private def checkValueOption(option: OptionArgument) = option.value match {
    case Some(pattern()) if ( valueRequired ) => Valid(option)
    case _ =>
      if ( !valueRequired ) Error(OptionValueNotSupported(option))
      else Error(OptionValueInvalid(option))
  }
}

case class SimpleRule(valuePattern: String = DefaultValuePattern) extends ArgumentRule with ValuePatternRule {
  def apply(simple: SimpleArgument) = simple.value match {
    case pattern() => Valid(simple)
    case x => Error(NonMatchingArgumentPattern(simple, valuePattern))
  }
}

case class FlagRule(character: Char,
                    parameterSpecification: Option[ArgumentSpecification] = None) extends ArgumentRule with ParameterisedRule {
  def apply(flag: FlagArgument) = parameterSpecification match {
    case None => Valid(flag)
    case Some(spec) => Expectation(flag, spec)
  }
}
