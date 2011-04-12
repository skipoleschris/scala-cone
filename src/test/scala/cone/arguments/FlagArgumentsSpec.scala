package cone.arguments

import org.specs2.Specification

/**
 * @author Chris Turner
 */
class FlagArgumentsSpec extends Specification { def is =

  "Specification to check the processing of flag arguments on the command line" ^
                                                               p^
  "The arguments '-a -b' should"                               ^
    "detect the a flag"                                        ! detectFlag("-a -b")^
    "detect the b flag"                                        ! detectFlag("-a -b")^
                                                               endp^
  "The arguments '-ab' should"                                 ^
    "detect the a flag"                                        ! detectFlag("-ab")^
    "detect the b flag"                                        ! detectFlag("-ab")^
                                                               endp^
  "The arguments '-h localhost' should"                        ^
    "detect the h flag"                                        ! detectFlag("-h localhost")^
    "record the value 'localhost' for the h flag"              ! recordValue("-h localhost")^
                                                               endp^
  "The arguments '-t one two three' should"                    ^
    "detect the t flag"                                        ! detectFlag("-t one two three")^
    "record the value 'one' at index 0 for the t flag"         ! recordValueAtPos("-t one two three")^
    "record the value 'two' at index 1 for the t flag"         ! recordValueAtPos("-t one two three")^
    "record the value 'three' at index 2 for the t flag"       ! recordValueAtPos("-t one two three")^
                                                               endp^
  "The arguments '-ha localhost' should"                       ^
    "detect the a flag"                                        ! detectFlag("-ha localhost")^
    "detect the h flag"                                        ! detectFlag("-ha localhost")^
    "record the value 'localhost' for the h flag"              ! recordValue("-ha localhost")^
                                                               endp^
  "The arguments '-p 12345' should"                            ^
    "detect the p flag"                                        ! detectFlag("-p 12345")^
    "record the value '12345' for the p flag"                  ! recordValue("-p 12345")^
                                                               endp^
  "The arguments '-h' should"                                  ^
    "report the h flag as an error"                            ! detectError("-h", InsufficientFlagParameters(FlagArgument('h')))^
                                                               endp^
  "The arguments '-hp localhost' should"                       ^
    "report the p flag as an error"                            ! detectError("-hp localhost", MultipleParameterExpectations(FlagArgument('p')))^
                                                               endp^
  "The arguments '-g' should"                                  ^
    "report the g flag as an error"                            ! detectError("-g", UnknownFlag(FlagArgument('g')))^
                                                               endp^
  "The arguments '-a -a' should"                               ^
    "report the a flag as an error"                            ! detectError("-a -a", DuplicateArgument(FlagArgument('a')))^
                                                               endp^
  "The arguments '-aa' should"                                 ^
    "report the a flag as an error"                            ! detectError("-aa", DuplicateArgument(FlagArgument('a')))^
                                                                endp^
  "The arguments '-h localhost -h remote' should"              ^
    "report the h flag as an error"                            ! detectError("-h localhost -h remote", DuplicateArgument(FlagArgument('h', List(SimpleArgument("remote")))))^
                                                               endp^
  "The arguments '-p bad' should"                              ^
    "report the p flag as an error"                            ! detectError("-p bad", InvalidFlagParameter(FlagArgument('p')))^
    "report the bad argument as an error"                            ! detectError("-p bad", NonMatchingArgumentPattern(SimpleArgument("bad"), "[0-9]+"))^
                                                               end

  val Detect = """^detect the ([a-zA-Z0-9]) flag""".r
  val Record = """^record the value '(.*)' for the ([a-zA-Z0-9]) flag$""".r
  val RecordAtPos = """^record the value '(.*)' at index (\d+) for the ([a-zA-Z0-9]) flag$""".r

  def detectFlag(args: String) = (s: String) => {
    val Detect(expectedFlag) = s
    processAndFindExpected(args, expectedFlag.head) must_!= None
  }

  def recordValue(args: String) = (s: String) => {
    val Record(value, flagString) = s
    val flag = flagString.head
    processAndFindExpected(args, flag) must_== Some(FlagArgument(flag, List(SimpleArgument(value))))
  }

  def recordValueAtPos(args: String) = (s: String) => {
    val RecordAtPos(value, position, flagString) = s
    val flag = flagString.head
    val index = position.toInt
    val param = processAndFindExpected(args, flag) match {
      case Some(FlagArgument(f, params)) if ( f == flag ) => Some(params.reverse.apply(index))
      case _ => None
    }
    param must_== Some(SimpleArgument(value))
  }

  def detectError(args: String, expectedError: ArgumentError) = processArguments(args) match {
    case Errors(errors) => errors must contain(expectedError)
    case a => sys.error("Expected an errors result, but got: " + a)
  }

  def processAndFindExpected(args: String, expectedFlag: Char) = processArguments(args) match {
    case Result(arguments) => arguments.asInstanceOf[List[Argument]].find(matchesExpected(expectedFlag))
    case e => sys.error("Expected a results set, but got: " + e)
  }

  def processArguments(args: String) = new ArgumentsProcessor(buildSpecification).apply(args.split(' '))

  def buildSpecification = ArgumentSpecification(
    List(),
    List(FlagRule('a'), FlagRule('b'),
         FlagRule('h', Some(ArgumentSpecification(List(), List(), List(SimpleRule())))),
         FlagRule('p', Some(ArgumentSpecification(List(), List(), List(SimpleRule("[0-9]+"))))),
         FlagRule('t', Some(ArgumentSpecification(List(), List(), List(SimpleRule(), SimpleRule(), SimpleRule()))))),
    List());

  def matchesExpected(expected: Char)(argument: Argument): Boolean = argument match {
    case FlagArgument(flag, _) if (flag == expected) => true
    case _ => false
  }
}
