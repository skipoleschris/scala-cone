package cone.arguments

/**
 * @author Chris Turner
 */
case class ArgumentAccumulator(processedArguments: List[Argument],
                               errors: List[ArgumentError],
                               argumentWithExpectations: Option[Argument],
                               expectationsRemaining: List[SimpleRule]) {

  def +(arg: Argument) =
    if ( isExpecting ) addExpected(arg)
    else copy(processedArguments = arg :: processedArguments)

  def +(error: ArgumentError) = copy(errors = error :: errors,
                                     argumentWithExpectations = None,
                                     expectationsRemaining = List())

  def expecting(arg: Argument, expectations: List[SimpleRule]) =
    if ( isExpecting ) copy(errors = MultipleParameterExpectations(arg) :: errors)
    else copy(argumentWithExpectations = Some(arg), expectationsRemaining = expectations)

  def isExpecting = argumentWithExpectations != None

  def addExpected(arg: Argument) = expectationsRemaining match {
    case Nil => sys.error("Unexpected expectation argument processed when none was expected")
    case x :: xs if ( xs == Nil ) => copy(argumentWithExpectations.get + arg :: processedArguments,
                                          argumentWithExpectations = None,
                                          expectationsRemaining = xs)
    case _ => copy(argumentWithExpectations = argumentWithExpectations.map(_ + arg),
                   expectationsRemaining = expectationsRemaining.tail)
  }

  def noOfSimpleArguments = processedArguments.filter(_.isInstanceOf[SimpleArgument]).length
}

object ArgumentAccumulator {

  def create = new ArgumentAccumulator(List(), List(), None, List())
}
