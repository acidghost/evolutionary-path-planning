package nl.vu.ai.acidghost.ec.ga

/**
 * Created by acidghost on 13/09/15.
 */
object Evolution {

    private val elitism = Configuration.getBoolean("elitism")
    private val uniformRate = Configuration.getDouble("uniformRate")
    private val mutationRate = Configuration.getDouble("mutationRate")
    private val tournamentSize = Configuration.getInt("tournamentSize")

    // Evolve the population
    def evolvePopulation(population: Population): Population = {
        val newPopulation = new Population(population.size)

        // Keep our best individual
        if (elitism) {
            newPopulation.setIndividual(0, population.getFittest)
        }

        // Crossover population
        val elitismOffset = if (elitism) 1 else 0

        // Loop over the population size and create new individuals with crossover
        for (i <- elitismOffset to population.size by 2) {
            val parent1 = tournamentSelection(population)
            val parent2 = tournamentSelection(population)
            val (offspring1, offspring2) = crossover(parent1, parent2)
            newPopulation.setIndividual(i, offspring1)
            newPopulation.setIndividual(i + 1, offspring2)
        }

        // Mutate population
        for (i <- elitismOffset to newPopulation.size) {
            mutate(newPopulation.getIndividual(i))
        }

        newPopulation
    }

    // Do tournament parent selection
    def tournamentSelection(population: Population): Individual = {
        // Create a tournament population
        val tournament = new Population(tournamentSize)
        // For each place in the tournament get a random individual
        for (i <- 0 to tournamentSize) {
            val random = Math.random() * population.size
            tournament.setIndividual(i, population.getIndividual(random.toInt))
        }
        // Get the fittest
        tournament.getFittest
    }

    // Generate offspring by crossover operation
    def crossover(parent1: Individual, parent2: Individual): (Individual, Individual) = {
        val offspring1, offspring2 = new Individual

        // Implement one-point crossover
        val minParents = Math.min(parent1.size, parent2.size)
        val point = Math.random() * minParents toInt
        val parent1Genes = (parent1.getGenes(0, point), parent1.getGenes(point + 1, minParents))
        val parent2Genes = (parent2.getGenes(0, point), parent2.getGenes(point + 1, minParents))

        offspring1.setGenes(0, point, parent1Genes._1)
        offspring1.setGenes(point + 1, minParents, parent2Genes._2)
        offspring2.setGenes(0, point, parent2Genes._1)
        offspring2.setGenes(point + 1, minParents, parent1Genes._2)

        (offspring1, offspring2)
    }

    // Mutate an individual. Do the following:
    //  - growth mutation
    //  - shrink mutation
    //  - swap mutation
    //  - replace mutation
    //  - parameter mutation
    //  - heuristic mutations
    def mutate(individual: Individual) = ???

}
