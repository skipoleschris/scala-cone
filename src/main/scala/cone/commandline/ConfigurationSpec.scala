package cone.commandline

/**
 * @author Chris Turner
 */
case class ConfigurationSpec(rules: Seq[ConfigurationOption]) {
  def ruleForFlag(flag: Char): Option[ConfigurationOption] = rules.find(_.forFlag == Some(flag))
  def ruleForOption(option: String): Option[ConfigurationOption] = rules.find(_.forOption == Some(option))
}

case class ConfigurationOption(name: String,
                               forFlag: Option[Char] = None,
                               forOption: Option[String] = None,
                               requiresValue: Boolean = false,
                               validationPattern: String = "(.*)") {
  private lazy val validationRegexp = validationPattern.r

  def applyOption(value: Option[String]) = value match {
    case None => noParameter
    case Some(v) =>
      if ( requiresValue ) validatedConfiguration(v)
      else ConfigurationParameterRequired(this)
  }

  def applyFlag(arguments: List[ArgumentElement]) = arguments match {
    case SimpleArgument(v) :: xs =>
      if ( requiresValue ) validatedConfiguration(v)
      else Configuration(this, None)
    case _ => noParameter
  }

  private def noParameter =
    if ( requiresValue ) ConfigurationParameterRequired(this)
     else Configuration(this, None)

  private def validatedConfiguration(value: String) = value match {
    case validationRegexp(v) => Configuration(this, Some(value))
    case _ => InvalidConfigurationParameterValue(this, value)
  }
}



