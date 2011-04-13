package cone.arguments

/**
 * @author Chris Turner
 */
case class ArgumentSpecification(optionRules: List[OptionRule],
                                 flagRules: List[FlagRule],
                                 simpleRules: List[SimpleRule] = List()) {

  def minRequiredArguments = simpleRules.size

  def applyRuleFor[T <: Argument](argument: T, simpleArgumentIndex: Int) = argument match {
    case option: OptionArgument => applyOptionRule(option)
    case flag: FlagArgument => applyFlagRule(flag)
    case simple: SimpleArgument => applySimpleRule(simple, simpleArgumentIndex)
  }

  private def applyOptionRule(option: OptionArgument) = optionRules.find(_.name == option.name) match {
    case None => Error(UnknownOption(option))
    case Some(rule) => rule(option)
  }


  private def applyFlagRule(flag: FlagArgument) = flagRules.find(_.character == flag.flag) match {
    case None => Error(UnknownFlag(flag))
    case Some(rule) => rule(flag)
  }

  private def applySimpleRule(simple: SimpleArgument, index: Int) = simpleRules match {
    case Nil => Valid(simple)
    case x if ( x.length > index ) => x(index)(simple)
    case _ => Error(TooManyArguments(simple))
  }
}
