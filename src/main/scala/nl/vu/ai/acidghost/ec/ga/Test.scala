package nl.vu.ai.acidghost.ec.ga

/**
 * Created by acidghost on 13/09/15.
 */
object Test extends App {

    for (i <- 1 to 10) {
        val individual = Individual.generateIndividual()
        println(individual)
    }

    println(Configuration.mapSize)

}
