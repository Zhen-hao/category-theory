object PureFunctions {

    case class FunctionBroker[A,B](fun: A => B){

        private var cache: Map[A,B] = Map()

        def apply(arg: A): B = {
            if (cache.isDefinedAt(arg)) cache(arg)
            else {
                val returnValue = fun(arg)
                cache = cache + (arg -> returnValue)
                returnValue
            }
        }
    }

}



object PureFunctionDemo extends App{

    import PureFunctions._

    def slowPlus1(n: Int): Int = {
        Thread.sleep(2000)
        n + 1
    }

    val fasterPlus1 = FunctionBroker(slowPlus1)

    println("first call")
    println(fasterPlus1(2))

    println("second call")
    println(fasterPlus1(2))

}