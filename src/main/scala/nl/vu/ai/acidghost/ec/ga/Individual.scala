package nl.vu.ai.acidghost.ec.ga

import nl.vu.ai.acidghost.ec.ga.Genotypes.Genotype

import scala.collection.mutable.ListBuffer

/**
 * Created by acidghost on 12/09/15.
 */
class Individual {

    import Genotypes._

    private val genes: ListBuffer[Genotype] = new ListBuffer[Genotype]

    def size: Int = genes.length

    def getGene(i: Int): Genotype = genes(i)

    def getGenes(from: Int, to: Int): List[Genotype] = {
        genes.toList.slice(from, to)
    }

    def setGene(i: Int, genotype: Genotype) = genes(i) = genotype

    def setGenes(from: Int, genesToCopy: List[Genotype]) = {
        if (from == 0 && genes.isEmpty) {
            genes ++= genesToCopy
        } else {
            if (from + genesToCopy.length > genes.length - 1) {
                val available = genes.length - from
                for (i <- from to genes.length - 1; j <- 0 to available) {
                    genes(i) = genesToCopy(j)
                }
                val start = genes.length - from
                for (i <- start to genesToCopy.length - 1) {
                    genes += genesToCopy(i)
                }
            } else {
                for (i <- from to from + genesToCopy.length - 1; j <- genesToCopy.indices) {
                    genes(i) = genesToCopy(j)
                }
            }
        }
    }

    def addGene(genotype: Genotype) = genes += genotype

    def getFitness: Double = FitnessCalc.getFitness(this)

    override def toString: String = genes.mkString("Chromosome: ", ", ", ".")

}

object Individual {

    private val genotypes = Genotypes.values.toArray

    // Create a random individual
    def generateIndividual(minSize: Int = Configuration.minInitial, maxSize: Int = Configuration.maxInitial, mapSize: (Int, Int) = Configuration.mapSize): Individual = {
        val individual = new Individual
        val size = Math.round(Math.random() * (maxSize - minSize - 1) + minSize).toInt
        var position = (1, 1)
        for (i <- 0 to size - 1) {
            // val gene = randomGene(genotypes)
            val genes = FitnessCalc.getValidGenotypes(position)
            val gene = randomGene(genes)
            individual.addGene(gene)
            position = FitnessCalc.getNewPosition(position, gene)
        }
        individual
    }

    def randomGene(genotypes: List[Genotype]): Genotype = {
        val randomGene = Math.round(Math.random() * (genotypes.length - 1)).toInt
        genotypes(randomGene)
    }

}
