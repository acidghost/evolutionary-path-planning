package nl.vu.ai.acidghost.ec.ga

import breeze.linalg.DenseVector
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
     *  - chromosome size minus first conflicting position (means-driven)
     *  - chromosome size (?) (favors longer chromosomes to avoid being stuck in local optima)
     *  - best sequence size
     *  - count_collisions(FinalAction) (?)
     *  - phenotype ends in goal position (goal-driven)
     *
     * @param individual The individual to compute the fitness for
     * @return The fitness score
     */
    def getFitness(individual: Individual, mapSize: (Int, Int) = Configuration.mapSize): Double = {
        val (conflictingActions, firstConflicting, bestSequence, endsInGoal, positions) = executeChromosome(individual)
        val steps = stepsFromGoal(positions.last)
        // println(individual.size, conflictingActions, firstConflicting, bestSequence, endsInGoal, positions.last)

//        (Math.pow(conflictingActions, 2) / mapSize._1) -
//          (Math.pow(individual.size, 2) / mapSize._1) +
//          ((individual.size - firstConflicting) * 6) -
//          (bestSequence * mapSize._1) -
//          (if (endsInGoal) Math.exp(mapSize._1 * 10) else 0) +
//          (Math.pow(stepsFromGoal(positions.last), 5) / mapSize._1)
//        val score = Math.pow(conflictingActions + 1, 2) -
//            Math.pow(individual.size - (firstConflicting + 2), 2) +
//            7 * Math.pow(steps, 2) +
//            5 * Math.pow(individual.size, 2) -
//            (if (endsInGoal) Math.pow(mapSize._1, 4) else 0) -
//            (if (steps <= mapSize._1 / 4) Math.pow(mapSize._1 * bestSequence, 2) else 0)

        val features = DenseVector(
            Math.pow(steps, 2.2),
            Math.pow(conflictingActions + 1, 2),
            - Math.pow(individual.size - (firstConflicting + 2), 2),
            Math.pow(individual.size, 2)
        )

        val weights = DenseVector(0.6, 0.2, 0.05, 0.15).t

        val score = weights * features

        // println(score)
        score // / mapSize._1
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
