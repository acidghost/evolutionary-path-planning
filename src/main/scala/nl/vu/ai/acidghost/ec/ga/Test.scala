package nl.vu.ai.acidghost.ec.ga

/**
 * Created by acidghost on 13/09/15.
 */
object Test extends App {

    val population = new Population(10, true)
    for (i <- 0 to population.size - 1) {
        println(population.getIndividual(i))
    }

    println(Configuration.mapSize)

}
