package cone.arguments

import org.specs2.Specification

/**
 * @author Chris Turner
 */
class OptionArgumentsSpec extends Specification { def is =

  "Specification to check the processing of option arguments on the command line" ^
                                                               p^
  "The arguments '--help --status' should"                     ^
    "detect the help option"                                   ! detectOption("--help --status")^
    "detect the status option"                                 ! detectOption("--help --status")^
                                                               endp^
  "The arguments '--host=localhost' should"                    ^
    "detect the host option"                                   ! detectOption("--host=localhost")^
    "record the value 'localhost' for the host option"         ! recordValue("--host=localhost")^
                                                               endp^
  "The arguments --password= should"                           ^
    "detect the password option"                               ! detectOption("--password=")^
    "record the value '' for the password option"              ! recordValue("--password=")^
                                                               endp^
  "The arguments '--port=12345' should"                        ^
    "detect the port option"                                   ! detectOption("--port=12345")^
    "record the value '12345' for the port option"             ! recordValue("--port=12345")^
                                                               endp^
  "The arguments '--port=bad' should"                          ^
    "report the port option as an error"                       ! detectError("--port=bad", OptionValueInvalid(OptionArgument("port", Some("bad"))))^
                                                               endp^
  "The arguments '--username=' should"                         ^
    "report the username option as an error"                   ! detectError("--username=", OptionValueInvalid(OptionArgument("username", Some(""))))^
                                                               endp^
  "The arguments '--username' should"                         ^
    "report the username option as an error"                   ! detectError("--username", OptionValueNotSupplied(OptionArgument("username", None)))^
                                                               endp^
  "The arguments '--unknown=value' should"                     ^
    "report the --unknown argument as an error"                ! detectError("--unknown=value", UnknownOption(OptionArgument("unknown", Some("value"))))^
                                                               endp^
  "The arguments '--help --help' should"                       ^
    "report the help option as an error"                       ! detectError("--help --help", DuplicateArgument(OptionArgument("help", None)))^
                                                               endp^
  "The arguments '--host=localhost --host=remote' should"      ^
    "report the host option as an error"                       ! detectError("--host=localhost --host=remote", DuplicateArgument(OptionArgument("host", Some("remote"))))^
                                                               end

  val Detect = """^detect the (.*) option$""".r
  val Record = """^record the value '(.*)' for the (.*) option$""".r

  def detectOption(args: String) = (s: String) => {
    val Detect(expectedOption) = s
    processAndFindExpected(args, expectedOption) must_!= None
  }

  def recordValue(args: String) = (s: String) => {
    val Record(value, option) = s
    processAndFindExpected(args, option) must_== Some(OptionArgument(option, Some(value)))
  }

  def detectError(args: String, expectedError: ArgumentError) =
    processArguments(args).errors must contain(expectedError)

  def processAndFindExpected(args: String, expectedOption: String) =
    processArguments(args).processedArguments.find(matchesExpected(expectedOption))

  def processArguments(args: String) = new ArgumentsProcessor(buildSpecification).processArguments(args.split(' '))

  def buildSpecification = ArgumentSpecification(
    List(OptionRule("help"), OptionRule("status"), OptionRule("host", true), OptionRule("password", true, ".*"),
         OptionRule("port", true, "[0-9]+"), OptionRule("username", true)),
    List(),
    List());

  def matchesExpected(expected: String)(argument: Argument): Boolean = argument match {
    case OptionArgument(name, _) if (name == expected) => true
    case _ => false
  }
}


