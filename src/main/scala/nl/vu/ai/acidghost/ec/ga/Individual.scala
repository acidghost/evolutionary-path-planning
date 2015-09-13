package nl.vu.ai.acidghost.ec.ga

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

    def setGenes(from: Int, to: Int, genes: List[Genotype]) = {
        for (i <- from to to) {
            this.genes(i) = genes(i)
        }
    }

    def addGene(genotype: Genotype) = genes += genotype

    def getFitness: Int = FitnessCalc.getFitness(this)

    override def toString: String = genes.mkString("Chromosome: ", ", ", ".")

}

object Individual {

    private val genotypes = Genotypes.values.toArray

    // Create a random individual
    def generateIndividual(maxSize: Int = 10): Individual = {
        val individual = new Individual
        val size = Math.round(Math.random() * (maxSize - 1) - 1).toInt
        for (i <- 1 to size) {
            val randomGene = Math.round(Math.random() * (genotypes.length - 1)).toInt
            val gene = genotypes(randomGene)
            individual.addGene(gene)
        }
        individual
    }

}
