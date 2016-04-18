package neat

import json.EvolutionParameters

case class Connection(from: Int, to: Int, weight: Int, disabled: Boolean, innovation: Int)

class Individual(val genome: List[Connection], var species: Species) {

	var fitness: Double = -1

	def this(clone: Individual) {
		this(clone.genome.clone(), clone.species)
	}



	def dist(individual: Individual)(implicit args: EvolutionParameters): Double = {
		var weightDifference = 0D
		var matchingGenes = 0
		var excessGenes = 0
		var disjointGenes = 0

		var i = genome
		var j = individual.genome

		val higher = List(i, j).maxBy(_.head.innovation)
		val lower = List(i, j).minBy(_.head.innovation)
		while(higher.head.innovation > lower.head.innovation) {
			excessGenes += 1
		}

		while(i.nonEmpty && j.nonEmpty) {
			if(i.head.innovation > j.head.innovation) {
				i = i.tail
				disjointGenes += 1
			}
			if(i.head.innovation == j.head.innovation) {
				weightDifference += (i.head.weight - j.head.weight).abs
				matchingGenes += 1
				i = i.tail
				j = j.tail
			}
			if(j.head.innovation > i.head.innovation) {
				j = j.tail
				disjointGenes += 1
			}
		}

		var result = (excessGenes * args.excessCoefficient + disjointGenes * args.disjointCoefficient) / individual.genome.size.max(genome.size)
		if(matchingGenes > 0)
			result += weightDifference * args.weightDifferenceCoefficient / matchingGenes
		result
	}


	def mate(dad: Individual)(implicit args: EvolutionParameters): Individual = {
		// TODO
	}

	def mutate()(implicit args: EvolutionParameters): Unit = {
		// TODO
	}

}
