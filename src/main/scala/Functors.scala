import cats.Id
import scala.language.higherKinds

object Functors {

    trait Functor[F[_]] {
        def lift[A, B](f: A => B): F[A] => F[B]
        def map[A, B](fa: F[A])(f: A => B): F[B] = lift(f)(fa)
    }

    object Functor{
        implicit def apply[F[_]](implicit functor: Functor[F]): Functor[F] = functor
    }

    trait DeepFunctor[X, A] {
        type Result[_]
        def map[B](x: X)(f: A => B): Result[B]
        def lift[B](f: A => B): X => Result[B] = map(_)(f)
    }

    object DeepFunctor {

        implicit def identityDeepFunctor[A]: DeepFunctor[A, A] =
            new DeepFunctor[A, A] {
                type Result[x] = Id[x]
                def map[B](x: A)(f: A => B) = f(x)
            }


        implicit def deep[F[_], X, A](implicit functorF: Functor[F], inner: DeepFunctor[X, A]) =
            new DeepFunctor[F[X], A] {
                type Result[x] = F[inner.Result[x]]
                def map[B](x: F[X])(f: A => B) = functorF.map(x)(inner.lift(f))
            }
    }

    implicit class Ops[X](self: X) {
        def fmap[A, B](f: A => B)(implicit F: DeepFunctor[X, A]) = F.map(self)(f)
    }


    // Example implementation for List
    implicit val listFunctor: Functor[List] = new Functor[List]{
        def lift[A, B](f: A => B): List[A] => List[B] = {
            case Nil => Nil
            case a :: tail => f(a) :: lift(f)(tail)
        }
    }


    // Example implementation for Option
    implicit val functorForOption: Functor[Option] = new Functor[Option] {
        def lift[A, B](f: A => B): Option[A] => Option[B] = {
            case None    => None
            case Some(a) => Some(f(a))
        }

    }


}


object FunctorDemo extends App{
    import Functors._

    val result = implicitly[Functor[Option]].lift((n: Int) => n * 2)(Some(200))
    val result1 = Functor[Option].lift((n: Int) => n * 2)(Some(200))


    val result2 = Some(200).asInstanceOf[Option[Int]] fmap ((n: Int) => n * 2)
    val result3 = Some(200).map(identity) fmap ((n: Int) => n * 2)


    val list0 = Functor[List].lift((n: Int) => n * 2)(List(1,2,3))
    val list = List(1,2,3) fmap ((n: Int) => n * 2)

    val composed = List(None,Some(2),Some(3)) fmap ((n: Int) => n * 2)


    val doubleCompose = List(None,Some(List(2)),Some(List()), None, Some(List(1,4,5))) fmap ((n: Int) => n * 2)

    print(doubleCompose)

}