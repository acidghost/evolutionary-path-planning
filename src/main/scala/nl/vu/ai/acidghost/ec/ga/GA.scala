package nl.vu.ai.acidghost.ec.ga

import scala.collection.mutable.ListBuffer

/**
 * Created by acidghost on 14/09/15.
 */
object GA extends App {

    // Create initial population
    val generations = new ListBuffer[Population]
    generations += new Population(Configuration.getInt("populationSize"), true)

    // Let evolution do the work
    for (i <- 1 to Configuration.getInt("maxGenerations")) {
        val generation = generations.last
        val fittest = generation.getFittest
        val exec = FitnessCalc.executeChromosome(fittest)
        println(s"Generation ${generations.length}\n" +
          s"Fittest: ${fittest.getFitness} ends in " +
          s"${exec._5.last} in ${fittest.size} steps\n" +
          s"$fittest\n")
        generations += Evolution.evolvePopulation(generation)
    }

    val lastFittest = generations.last.getFittest
    val exec = FitnessCalc.executeChromosome(lastFittest)
    println(s"Fittest solution:\n$lastFittest\nFitness: ${lastFittest.getFitness} ends in ${exec._5.last}")

    import org.sameersingh.scalaplot.Implicits._

    val x = 1 to generations.length map(_.toDouble)
    println(output(ASCII, xyChart(x -> generations.map(_.getFittest.getFitness))))

}
