package cone.arguments

/**
 * @author Chris Turner
 */
case class ArgumentAccumulator(processedArguments: List[Argument],
                               errors: List[ArgumentError],
                               argsCount: Int = 0,
                               argsUsed: Int = 0,
                               childrenMerged: Boolean = false) {

  def +(arg: Argument) = checkForDuplicateAndApply(arg)(addProcessedArgument)

  private def addProcessedArgument(a: Argument, acc: ArgumentAccumulator) =
      acc.copy(processedArguments = a :: acc.processedArguments, argsCount = argsCount + 1)

  def +(error: ArgumentError) = copy(errors = error :: errors)

  def check(expectedArguments: Int) = {
    val actual = processedArguments.filter(_.isInstanceOf[SimpleArgument]).size
    if ( actual < expectedArguments ) this + InsufficientArguments(expectedArguments, actual)
    else this
  }

  def checkNested(argument: Parameterised, expectedArguments: Int) = {
    val actual = processedArguments.filter(_.isInstanceOf[SimpleArgument]).size
    if ( actual < expectedArguments ) this + InsufficientFlagParameters(argument)
    else this
  }

  private def checkForDuplicateAndApply(arg: Argument)(f: (Argument, ArgumentAccumulator) => ArgumentAccumulator) =
    processedArguments.find(isDuplicate(arg)) match {
      case None => f(arg, this)
      case _ => copy(errors = DuplicateArgument(arg) :: errors)
    }

  private def isDuplicate(arg: Argument)(checkWith: Argument) = arg.isDuplicateOf(checkWith)

  def hasErrors = errors.size > 0

  def noOfSimpleArguments = processedArguments.filter(_.isInstanceOf[SimpleArgument]).length

  def mergeErrors(acc: ArgumentAccumulator) = copy(errors = acc.errors ++ errors,
                                                   argsUsed = acc.argsUsed)

  def mergeChildren(argument: Parameterised, childAcc: ArgumentAccumulator) =
    checkForDuplicateAndApply(argument ++ childAcc.processedArguments)(mergeFromChildAccumulator(childAcc))

  def mergeFromChildAccumulator(childAcc: ArgumentAccumulator)(a: Argument, acc: ArgumentAccumulator) =
    acc.copy(processedArguments = a :: acc.processedArguments,
             argsUsed = childAcc.argsCount, childrenMerged = true)

  def flagsProcessed = copy(childrenMerged = false)
}

object ArgumentAccumulator {

  def create = new ArgumentAccumulator(List(), List(), 0)
}
