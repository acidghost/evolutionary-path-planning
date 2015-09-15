package nl.vu.ai.acidghost.ec.ga

/**
 * Created by acidghost on 13/09/15.
 */
object Evolution {

    private val elitism = Configuration.getBoolean("elitism")
    private val uniformRate = Configuration.getDouble("uniformRate")
    private val growthRate = Configuration.getDouble("mutation.growthRate")
    private val shrinkRate = Configuration.getDouble("mutation.shrinkRate")
    private val swapRate = Configuration.getDouble("mutation.swapRate")
    private val replaceRate = Configuration.getDouble("mutation.replaceRate")
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
        for (i <- elitismOffset to population.size - 1 by 2) {
            val parent1 = tournamentSelection(population)
            val parent2 = tournamentSelection(population)
            val (offspring1, offspring2) = crossover(parent1, parent2)
            newPopulation.setIndividual(i, offspring1)
            newPopulation.setIndividual(i + 1, offspring2)
        }

        // Mutate population
        for (i <- elitismOffset to newPopulation.size - 1) {
            mutate(newPopulation.getIndividual(i))
        }

        newPopulation
    }

    // Do tournament parent selection
    def tournamentSelection(population: Population): Individual = {
        // Create a tournament population
        val tournament = new Population(tournamentSize)
        // For each place in the tournament get a random individual
        for (i <- 0 to tournamentSize - 1) {
            val random = Math.round(Math.random() * (population.size - 1)).toInt
            tournament.setIndividual(i, population.getIndividual(random))
        }
        // Get the fittest
        tournament.getFittest
    }

    // Generate offspring by crossover operation
    def crossover(parent1: Individual, parent2: Individual): (Individual, Individual) = {
        val offspring1, offspring2 = new Individual

        // Implement one-point crossover
        val minParents = Math.min(parent1.size, parent2.size)
        val point = Math.round(Math.random() * (minParents - 1)).toInt
        val parent1Genes = (parent1.getGenes(0, point + 1), parent1.getGenes(point + 1, parent1.size))
        val parent2Genes = (parent2.getGenes(0, point + 1), parent2.getGenes(point + 1, parent2.size))

        // println(s"Point in crossover $point where min $minParents ${parent1.size} ${parent2.size}")
        // println(s"\n${parent1Genes._1.length} ${parent1Genes._2.length} - ${parent2Genes._1.length} ${parent2Genes._2.length}")

        offspring1.setGenes(0, parent1Genes._1)
        offspring1.setGenes(parent1Genes._1.length, parent2Genes._2)
        offspring2.setGenes(0, parent2Genes._1)
        offspring2.setGenes(parent2Genes._1.length, parent1Genes._2)

        // println(s"Offsprings ${offspring1.size} ${offspring2.size}")

        (offspring1, offspring2)
    }

    /**
     * Mutate an individual. Do the following:
     *  - growth mutation
     *  - shrink mutation
     *  - swap mutation
     *  - replace mutation
     *  - <s>parameter mutation</s>
     *  - <s>heuristic mutation</s>
     *
     * @param individual The individual to mutate
     */
    def mutate(individual: Individual) = {
        val random = Math.random()
        if (random <= growthRate) {
            growthMutation(individual)
        }
        if (random <= shrinkRate) {
            shrinkMutation(individual)
        }
//        if (Math.random() <= swapRate) {
//            swapMutation(individual)
//        }
//        if (Math.random() <= replaceRate) {
//            replaceMutation(individual)
//        }
    }

    private def growthMutation(individual: Individual) = {
        val randomPoint = Math.round(Math.random() * (individual.size - 1)).toInt
        val gene = FitnessCalc.getValidGenotypesByExec(individual, randomPoint) match {
            case validGenes => validGenes(Math.round(Math.random() * (validGenes.length - 1)).toInt)
        }
        // Shift genes from randomPoint to end one right
        val genes = individual.getGenes(randomPoint, individual.size)
        // println(s"Growth $randomPoint ${individual.size} ${gene :: genes length}")
        individual.setGenes(randomPoint, gene :: genes)
    }

    private def shrinkMutation(individual: Individual) = {
        val randomPoint = Math.round(Math.random() * (individual.size - 1)).toInt
        // Shift genes from randomPoint + 1 till end one left
        val genes = individual.getGenes(randomPoint + 1, individual.size - 1)
        individual.setGenes(randomPoint, genes)
    }

    private def swapMutation(individual: Individual) = ???

    private def replaceMutation(individual: Individual) = ???

}
