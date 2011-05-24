package cone.commandline

/**
 * @author Chris Turner
 */
sealed trait ConfigurationElement
case class Configuration(option: ConfigurationOption, value: Option[String]) extends ConfigurationElement

sealed trait ConfigurationError extends ConfigurationElement
case class UnknownConfigurationOption(option: String) extends ConfigurationError
case class ConfigurationParameterRequired(option: ConfigurationOption) extends ConfigurationError
case class DuplicateConfigurationOption(option: ConfigurationOption) extends ConfigurationError
case class InvalidConfigurationParameterValue(option: ConfigurationOption, value: String) extends ConfigurationError

case class ProcessedConfigurations(configurations: List[Configuration] = List(),
                                   errors: List[ConfigurationError] = List(),
                                   arguments: List[String] = List()) {
	def +(value: String) = copy(arguments = arguments ++ List(value))

	def +(configuration: Configuration) =
    if ( isDuplicate(configuration) ) copy(errors = DuplicateConfigurationOption(configuration.option) :: errors)
    else copy(configurations = configuration :: configurations)

  def +(error: ConfigurationError) = copy(errors = error :: errors)

  private def isDuplicate(configuration: Configuration) = configurations.find(_.option == configuration.option) != None
}