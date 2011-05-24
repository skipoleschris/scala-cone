package cone.commandline

import org.specs2.Specification

/**
 * @author Chris Turner
 */
class ArgumentsTransformerSpec extends Specification { def is =

  "Specification to check the transformation of a command line" ^
                                                                p^
  "The argument '--help' should"                                ^
    "be transformed into an option argument with no vaue"       ! checkOption("--help", "help", None)^
                                                                endp^
  "The argument '--host=localhost' should"                      ^
    "be transformed into an option argument with a value"       ! checkOption("--host=localhost", "host", Some("localhost"))^
                                                                endp^
  "The argument '--host=' should"                               ^
    "be transformed into an option argument with no value"      ! checkOption("--host=", "host", None)^
                                                                endp^
  "The argument '-f' should"                                    ^
    "be transformed into a flag argument"                       ! checkFlag("-f", 'f')^
                                                                endp^
  "The argument '-cUv' should"                                  ^
    "be transformed into a flag group with three flags"         ! checkFlagGroupSize("-cUv", List('c', 'U', 'v'))^
                                                                endp^
  "The argument 'test' should"                                  ^
    "be transformed into a simple argument"                     ! checkSimple("test")^
                                                                endp^
  "The arguments '--host=localhost --cVf test.dat -g --quick hello world' should" ^
    "be transformed into a sequence of arguments"               ! checkFullCommandLine^
                                                                end

  def checkOption(arg: String, optionName: String, optionValue: Option[String]) =
    transform(arg) must_==  List(OptionArgument(optionName, optionValue))

  def checkFlag(arg: String, flagChar: Char) = transform(arg) must_== List(FlagArgument(flagChar))

  def checkFlagGroupSize(arg: String, flagChars: List[Char]) =
    transform(arg) must_==  List(FlagGroup(flagChars.map(FlagArgument(_))))

  def checkSimple(arg: String) =
    transform(arg) must_== List(SimpleArgument(arg))

  def checkFullCommandLine = {
    val transformer = new ArgumentsTransformer
    val result = transformer.transform(Array("--host=localhost", "-cVf", "test.dat", "-g", "--quick", "hello", "world"))
    result must_== List(OptionArgument("host", Some("localhost")),
                        FlagGroup(List(FlagArgument('c'), FlagArgument('V'), FlagArgument('f'))),
                        SimpleArgument("test.dat"),
                        FlagArgument('g'),
                        OptionArgument("quick", None),
                        SimpleArgument("hello"),
                        SimpleArgument("world"))
  }

  private def transform(arg: String) = {
    new ArgumentsTransformer().transform(Array(arg))
  }
}