package nl.vu.ai.acidghost.ec.ga

/**
 * Created by acidghost on 13/09/15.
 */
class Population(val populationSize: Int, initialize: Boolean = false) {

    private val individuals: Array[Individual] = new Array(populationSize)

    if (initialize) {
        for (i <- 0 to populationSize - 1) {
            individuals(i) = Individual.generateIndividual()
        }
    }

    def setIndividual(index: Int, individual: Individual) = individuals(index) = individual

    def getIndividual(index: Int) = individuals(index)

    def size = individuals.length

    // Fittest is the individual with lowest fitness value
    def getFittest: Individual = {
        var fittest = individuals.head
        for (i <- 0 to size - 1) {
            if (fittest.getFitness > individuals(i).getFitness) {
                fittest = individuals(i)
            }
        }
        fittest
    }

}
