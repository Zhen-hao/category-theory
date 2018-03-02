object Coproduct {


    sealed trait Either[A, B]

    case class Left[A, B](a: A) extends Either[A, B]
    case class Right[A, B](b: B) extends Either[A, B]

    def leftIn[A,B](a: A): Either[A, B] = Left(a)

    def rightIn[A,B](b: B): Either[A, B] = Right(b)

    def factorizer[A,B,C]: (A => C) => (B => C) => (Either[A,B] => C) =
        i => j =>  {
            case Left(a) => i(a)
            case Right(b) => j(b)
        }
}


object runCoproduct extends App{

    import Coproduct._

    val n = leftIn[Int, Boolean](5)

    println(n)

    val b = rightIn[Int, Boolean](true)

    println(b)

}