package cone.arguments

/**
 * @author Chris Turner
 */
object Arguments {
  def result[A](arguments: A) = new Result[A](arguments)
  def errors(errors: List[ArgumentError]) = new Errors(errors)
}

sealed abstract class Arguments[+A] {

  def result: A

  def map[B](f: A => B) = if ( isErrors ) this else Result[B](f(result))

  def flatMap[B](f: A => Arguments[B]) = if ( isErrors ) this else f(result)

  protected def isErrors: Boolean
}

case class Result[+A](arguments: A) extends Arguments[A] {
  def result = arguments
  protected def isErrors = false
}

case class Errors(errors: List[ArgumentError]) extends Arguments[List[ArgumentError]] {
  def result = errors
  protected def isErrors = true
}

