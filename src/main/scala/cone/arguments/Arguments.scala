package cone.arguments

/**
 * @author Chris Turner
 */
object Arguments {
  def result[A](arguments: A) = new Result[A](arguments)
  def errors(errors: List[ArgumentError]) = new Errors(errors)
}

abstract class Arguments[+A] {
  def result: A
}

case class Result[+A](arguments: A) extends Arguments[A] {
  def result = arguments
}

case class Errors(errors: List[ArgumentError]) extends Arguments[List[ArgumentError]] {
  def result = errors
}

