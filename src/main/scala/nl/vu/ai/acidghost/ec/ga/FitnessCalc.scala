package nl.vu.ai.acidghost.ec.ga

import nl.vu.ai.acidghost.ec.ga.Genotypes.Genotype

import scala.collection.mutable.ListBuffer

/**
 * Created by acidghost on 13/09/15.
 */
object FitnessCalc {

    /**
     * Get valid genotypes given current position in map and map information
     * 
     * @param position The current position in map
     * @param mapSize The map grid size
     * @return A list of available genotypes
     */
    def getValidGenotypes(position: (Int, Int), mapSize: (Int, Int) = Configuration.mapSize): List[Genotype] = {
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

    def getValidGenotypesByExec(individual: Individual, point: Int, mapSize: (Int, Int) = Configuration.mapSize): List[Genotype] = {
        val positions = executeChromosome(individual, mapSize)._5
        getValidGenotypes(positions(point), mapSize)
    }

    /**
     * Calculates new position in map given actual position and action
     * 
     * Y-axis vertical directed toward bottom
     * X-axis horizontal directed toward right
     * 
     * @param position The current position in map
     * @param action The action to be executed
     * @return
     */
    def getNewPosition(position: (Int, Int), action: Genotype): (Int, Int) = {
        action match {
            case Genotypes.North => (position._1, position._2 - 1)
            case Genotypes.South => (position._1, position._2 + 1)
            case Genotypes.East => (position._1 + 1, position._2)
            case Genotypes.West => (position._1 - 1, position._2)
        }
    }

    /**
     * Computes the fitness for an individual
     *
     * Composed of:
     *  - number of actions that cannot be executed
     *  - chromosome size minus first conflicting position
     *  - chromosome size (?) (favors longer chromosomes to avoid being stuck in local optima)
     *  - best sequence size
     *  - count_collisions(FinalAction) (?)
     *  - phenotype ends in goal position
     *
     * @param individual The individual to compute the fitness for
     * @return The fitness score
     */
    def getFitness(individual: Individual): Double = {
        val (conflictingActions, firstConflicting, bestSequence, endsInGoal, positions) = executeChromosome(individual)

        Math.exp(conflictingActions) -
          0.5 * individual.size +
          (individual.size - firstConflicting) -
          2 * bestSequence -
          (if (endsInGoal) 2 * individual.size else 0) +
          3 * stepsFromGoal(positions.last)
    }

    def executeChromosome(individual: Individual, mapSize: (Int, Int) = Configuration.mapSize) = {
        var position = (1, 1)
        var conflictingActions = 0
        var firstConflicting = -1
        var currentSequence = 0
        var bestSequence = 0
        val positions = new ListBuffer[(Int, Int)]
        positions += position
        for (i <- 0 to individual.size - 1) {
            getNewPosition(position, individual.getGene(i)) match {
                case newPosition: (Int, Int) if isValidPosition(newPosition) =>
                    position = newPosition
                    positions += position
                    currentSequence += 1
                case _ =>
                    positions += position
                    conflictingActions += 1
                    if (conflictingActions == 1) {
                        firstConflicting = i
                    }
                    if (currentSequence > bestSequence) {
                        bestSequence = currentSequence
                    }
                    currentSequence = 0
            }
        }
        (conflictingActions, firstConflicting, bestSequence, position == mapSize, positions.toList)
    }

    private def stepsFromGoal(position: (Int, Int), mapSize: (Int, Int) = Configuration.mapSize) = {
        (mapSize._1 - position._1) + (mapSize._2 - position._2)
    }

    private def isValidPosition(position: (Int, Int), mapSize: (Int, Int) = Configuration.mapSize) = {
        position._1 >= 1 && position._1 <= mapSize._1 && position._2 >= 1 && position._2 <= mapSize._2
    }

}
