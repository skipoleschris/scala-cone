package cone.arguments

/**
 * @author Chris Turner
 */
private[arguments] object Arguments {
  def result[A](arguments: Seq[A]) = new Result[A](arguments)
  def errors(errors: List[ArgumentError]) = new Errors(errors)
}

sealed abstract class Arguments[+A] {

  def result: Seq[A]

  def map[B](f: Seq[A] => Seq[B]) = if ( isErrors ) this else Result[B](f(result))

  def flatMap[B](f: Seq[A] => Arguments[B]) = if ( isErrors ) this else f(result)

  def mapContents[B](f: A => B) = if ( isErrors) this else Result[B](result.map(f))

  protected def isErrors: Boolean
}

case class Result[+A](arguments: Seq[A]) extends Arguments[A] {
  def result = arguments
  protected def isErrors = false
}

case class Errors(errors: List[ArgumentError]) extends Arguments[ArgumentError] {
  def result = errors
  protected def isErrors = true
}

