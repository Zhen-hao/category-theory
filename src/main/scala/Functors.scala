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


    // Example implementation for Reader
    class ReaderFunctor[R]{
        type M[x] = R => x

        implicit val functor: Functor[M] = new Functor[M]{
            def lift[A, B](f: A => B): M[A] => M[B] = f.compose
        }
    }

    object ReaderFunctor{
        implicit def apply[R]: ReaderFunctor[R] = new ReaderFunctor[R]
    }



    trait Bifunctor[F[_,_]]{

        def bimap[A,B,C,D](ac: A => C)(bd: B => D): F[A,B] => F[C,D]
            =  first[A,D,C](ac) compose second[A,B,D](bd)

        def first[A,B,C]: (A => C) => F[A,B] => F[C,B]
            = (g) => bimap(g)(identity)

        def second[A,B,D]: (B => D) => F[A,B] => F[A,D]
        = bimap(identity)

    }


    trait Profunctor[F[_,_]]{

        def dimap[A,B,C,D](ab: A => B)(cd: C => D): F[B,C] => F[A,D]
        =  first[A,B,D](ab) compose second[B,C,D](cd)

        def first[A,B,C]: (A => B) => F[B,C] => F[A,C]
        = (g) => dimap(g)(identity)

        def second[A,B,C]: (B => C) => F[A,B] => F[A,C]
        = dimap(identity)

    }


    case class Pair[A,B](a: A, b: B)

    object Bifunctor{

        implicit def pairBifunctor: Bifunctor[Pair] = new Bifunctor[Pair] {
            override def bimap[A, B, C, D](ac: A => C)(bd: B => D): Pair[A, B] => Pair[C, D] = {
                case Pair(a, b) => Pair(ac(a), bd(b))
            }
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

    println(doubleCompose)


    val readerFunctor = ReaderFunctor[String].functor
    println(readerFunctor.lift((x:Int) => x * 2))

    //val pairFunctor: Bifunctor[Pair] = Bifunctor.pairBifunctor
    //println(pairFunctor.bimap((a: Int) => a * 2)((b: Long) => b +2)(Pair(5, 15L)))

    //val pairFunctor = implicitly[Bifunctor[Pair]]
    println(implicitly[Bifunctor[Pair]].bimap((a: Int) => a * 2)((b: Long) => b +2)(Pair(5, 15L)))

}