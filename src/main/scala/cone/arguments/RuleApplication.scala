package cone.arguments

/**
 * @author Chris Turner
 */
trait RuleApplication

case class Valid(argument: Argument) extends RuleApplication
case class Error(cause: ArgumentError) extends RuleApplication
case class Expectation(argument: Argument, specification: ArgumentSpecification)

