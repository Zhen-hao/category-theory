object Identities {

    def id[A]: A => A = (x: A) => x

}


object IdentityDemo extends App{

    import Identities._

    val plusOne_id: Int => Int = ((n: Int) => n + 1) compose id

    val id_plusOne: Int => Int = id compose ((n: Int) => n + 1)

    println("plusOne_id applying to 1 to 100")
    (1 to 100).map(plusOne_id).foreach(println)

    println("id_plusOne applying to 1 to 100")
    (1 to 100).map(id_plusOne).foreach(println)




}