package cone.commandline

import org.specs2.Specification

/**
 * @author Chris Turner
 */
class ProcessedConfigurationsSpec extends Specification { def is =

  "Specification to check the processed configurations class"        ^
                                                                     p^
  "Adding a new value to the configurations should"                  ^
    "result in configurations containing the added value"            ! valueAdded^
                                                                     endp^
  "Adding a configuration to the configurations should"              ^
    "result in configurations containing the added configurations"   ! configurationAdded^
                                                                     endp^
  "Adding an error to the configurations should"                     ^
    "result in configurations containing the added error"            ! errorAdded^
                                                                     endp^
  "Adding a configuration that already exists should"                ^
    "result in configurations containing a duplicate configuration option error" ! duplicateConfigurationAdded^
                                                                     end

  def valueAdded = {
    val toCheck = ProcessedConfigurations() + "test"
    toCheck must_==  ProcessedConfigurations(arguments = List("test"))
  }

  def configurationAdded = {
    val option = ConfigurationOption("test")
    val toCheck = ProcessedConfigurations() + Configuration(option, None)
    toCheck must_== ProcessedConfigurations(configurations = List(Configuration(option, None)))
  }

  def errorAdded = {
    val toCheck = ProcessedConfigurations() + UnknownConfigurationOption("test")
    toCheck must_== ProcessedConfigurations(errors = List(UnknownConfigurationOption("test")))
  }

  def duplicateConfigurationAdded = {
    val option = ConfigurationOption("test")
    val toCheck = ProcessedConfigurations() + Configuration(option, None) + Configuration(option, None)
    toCheck must_== ProcessedConfigurations(configurations = List(Configuration(option, None)),
                                            errors = List(DuplicateConfigurationOption(option)))
  }
}
