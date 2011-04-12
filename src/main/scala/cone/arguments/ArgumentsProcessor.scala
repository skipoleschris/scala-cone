package cone.arguments

import annotation.tailrec

/**
 * @author Chris Turner
 */
class ArgumentsProcessor(specification: ArgumentSpecification, nested: Boolean = false) {

  private lazy val BooleanOptionPattern = """^\-\-([a-zA-Z0-9_\-]+)$""".r
  private lazy val ParameterOptionPattern = """^\-\-([a-zA-Z0-9_\-]+)(=(.*))?$""".r
  private lazy val FlagPattern = """^\-([a-zA-Z0-9]+)$""".r

  import Arguments._

  def apply(args: Array[String]) = {
    val processed = processArguments(args.toList).check(specification.minRequiredArguments)
    if ( processed.hasErrors ) errors(processed.errors.reverse)
    else result(processed.processedArguments.reverse)
  }

  private def processArguments(args: List[String]) = processNextArgument(args, ArgumentAccumulator.create)


  @tailrec
  private def processNextArgument(args: List[String], acc: ArgumentAccumulator): ArgumentAccumulator = args match {
    case Nil => acc
    case x::xs => {
      val next = processArgument(acc, x, xs)
      if ( nested && (next.noOfSimpleArguments == specification.simpleRules.size) ) next
      else processNextArgument(xs.drop(next.argsUsed), next.copy(argsUsed = 0))
    }
  }

  private def processArgument(acc: ArgumentAccumulator, arg: String, remainder: List[String]) = arg match {
    case BooleanOptionPattern(name) => applyArgument(OptionArgument.create(name, null), remainder, acc)
    case ParameterOptionPattern(name, _, value) => applyArgument(OptionArgument.create(name, value), remainder, acc)
    case FlagPattern(flags) => {
      println("***FLAGS: " + flags)
      val next = flags.foldLeft(acc)(processFlag(remainder)_)
      next.flagsProcessed
    }
    case _ => applyArgument(SimpleArgument(arg), remainder, acc)
  }

  private def processFlag(remainder: List[String])(acc: ArgumentAccumulator, c: Char) = {
    println("Process flag: " + c + " acc is: " + acc)
    applyArgument(FlagArgument(c), remainder, acc)
  }

  private def applyArgument(argument: Argument, remainder: List[String], acc: ArgumentAccumulator) =
    specification.applyRuleFor(argument, acc.noOfSimpleArguments) match {
      case Valid(arg) => acc + arg
      case Expectation(flag, spec) => {
        if ( acc.childrenMerged ) acc + MultipleParameterExpectations(flag)
        else {
          val childAccumulator = new ArgumentsProcessor(spec, true).processArguments(remainder)
          if ( childAccumulator.hasErrors ) acc.mergeErrors(childAccumulator) + InvalidFlagParameter(flag)
          else {
             val checkedAcc = childAccumulator.checkNested(flag, spec.simpleRules.size)
             if ( checkedAcc.hasErrors ) checkedAcc
             else acc.mergeChildren(flag, childAccumulator)
          }
        }
      }
      case Error(cause) => acc + cause
    }
}
