package cone.arguments

/**
 * @author Chris Turner
 */
trait ArgumentRule

case class OptionRule(name: String,
                      valueRequired: Boolean = false,
                      valuePattern: String = ".+") extends ArgumentRule {

   private lazy val pattern = valuePattern.r

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

case class SimpleRule(valuePattern: String = ".+",
                      mandatory: Boolean = true) extends ArgumentRule {

  private lazy val pattern = valuePattern.r

  def apply(simple: SimpleArgument) = simple.value match {
    case pattern() => Valid(simple)
    case x => Error(NonMatchingArgumentPattern(simple, valuePattern))
  }
}

case class FlagRule(character: Char, parameterSpecification: Option[ArgumentSpecification] = None) extends ArgumentRule {
  def apply(flag: FlagArgument) = parameterSpecification match {
    case None => Valid(flag)
    case Some(spec) => Expectation(flag, spec)
  }
}
