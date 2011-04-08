package cone.arguments

import org.specs2.Specification

/**
 * @author Chris Turner
 */
class SimpleArgumentsSpec extends Specification { def is =

  "Specification to check the processing of simple arguments on the command line" ^
                                                               p^
  "The arguments 'hello world' with no rules should"           ^
    "record the 'hello' argument"                              ! recordValue("hello world", noRules)^
    "record the 'world' argument"                              ! recordValue("hello world", noRules)^
                                                               endp^
  "The arguments 'hello world' with valid rules should"        ^
    "record the 'hello' argument"                              ! recordValue("hello world", patternRules)^
    "record the 'world' argument"                              ! recordValue("hello world", patternRules)^
                                                               endp^
  "The arguments '12345' with a specific rule should"          ^
    "record the '12345' argument"                              ! recordValue("12345", specificRule)^
                                                               endp^
  "The arguments 'bad' with a specific rule should"            ^
    "report the 'bad' argument as an error"                    ! detectError("bad", specificRule,
                                                                             NonMatchingArgumentPattern(SimpleArgument("bad"), "[0-9]+"))^
                                                               endp^
  "The arguments 'hello world again' with valid rules for two arguments should" ^
    "report the 'again' argument as an error"                  ! detectError("hello world again", patternRules,
                                                                             TooManyArguments(SimpleArgument("again")))^
                                                               endp^
  "The arguments 'hello' with valid rules for two argument should" ^
    "report an insufficient arguments error"                   ! detectError("hello", patternRules,
                                                                             InsufficientArguments(2, 1))^
                                                               end

  val Record = """^record the '(.*)' argument$""".r

  def noRules = List[SimpleRule]()
  def patternRules = List(SimpleRule(), SimpleRule())
  def specificRule = List(SimpleRule("[0-9]+"))

  def recordValue(args: String, rules: List[SimpleRule]) = (s: String) => {
    val Record(value) = s
    processAndFindExpected(args, value, rules) must_== Some(SimpleArgument(value))
  }

  def detectError(args: String, rules: List[SimpleRule], expectedError: ArgumentError) =
    processArguments(args, rules).errors must contain(expectedError)

  def processAndFindExpected(args: String, expectedValue: String, rules: List[SimpleRule]) =
    processArguments(args, rules).processedArguments.find(matchesExpected(expectedValue))

  def processArguments(args: String, rules: List[SimpleRule]) =
    new ArgumentsProcessor(buildSpecification(rules)).processArguments(args.split(' '))

  def buildSpecification(rules: List[SimpleRule]) = ArgumentSpecification(List(), List(), rules);

  def matchesExpected(expected: String)(argument: Argument): Boolean = argument match {
    case SimpleArgument(value) if (value == expected) => true
    case _ => false
  }
}