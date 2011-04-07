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
    else checkForDuplicateAndApply(arg)(addProcessedArgument)

  private def addProcessedArgument(a: Argument, acc: ArgumentAccumulator) =
      acc.copy(processedArguments = a :: acc.processedArguments)

  def +(error: ArgumentError) = copy(errors = error :: errors,
                                     argumentWithExpectations = None,
                                     expectationsRemaining = List())

  def expecting(arg: Argument, expectations: List[SimpleRule]) =
    if ( isExpecting ) copy(errors = MultipleParameterExpectations(arg) :: errors)
    else copy(argumentWithExpectations = Some(arg), expectationsRemaining = expectations)

  def isExpecting = argumentWithExpectations != None

  private def addExpected(arg: Argument) = expectationsRemaining match {
    case Nil => sys.error("Unexpected expectation argument processed when none was expected")
    case x :: xs if ( xs == Nil ) => copy(argumentWithExpectations.get + arg :: processedArguments,
                                          argumentWithExpectations = None,
                                          expectationsRemaining = xs)
    case _ => copy(argumentWithExpectations = argumentWithExpectations.map(_ + arg),
                   expectationsRemaining = expectationsRemaining.tail)
  }

  private def checkForDuplicateAndApply(arg: Argument)(f: (Argument, ArgumentAccumulator) => ArgumentAccumulator) =
    processedArguments.find(isDuplicate(arg)) match {
      case None => f(arg, this)
      case _ => copy(errors = DuplicateArgument(arg) :: errors)
    }

  private def isDuplicate(arg: Argument)(checkWith: Argument) = arg.isDuplicateOf(checkWith)

  def noOfSimpleArguments = processedArguments.filter(_.isInstanceOf[SimpleArgument]).length
}

object ArgumentAccumulator {

  def create = new ArgumentAccumulator(List(), List(), None, List())
}
