package nl.vu.ai.acidghost.ec.ga

import nl.vu.ai.acidghost.ec.ga.Genotypes.Genotype

import scala.collection.mutable.ListBuffer

/**
 * Created by acidghost on 13/09/15.
 */
object FitnessCalc {

    def getValidGenes(position: (Int, Int), mapSize: (Int, Int) = Configuration.mapSize): List[Genotype] = {
        val validActions = new ListBuffer[Genotype]

        if (position._1 >= 1 && position._1 < mapSize._1) {
            validActions += Genotypes.East
        }
        if (position._2 >= 1 && position._2 < mapSize._2) {
            validActions += Genotypes.South
        }
        if (position._1 > 1 && position._1 <= mapSize._1) {
            validActions += Genotypes.West
        }
        if (position._2 > 1 && position._2 <= mapSize._2) {
            validActions += Genotypes.North
        }
        validActions.toList
    }


    def getFitness(individual: Individual): Int = ???

}
