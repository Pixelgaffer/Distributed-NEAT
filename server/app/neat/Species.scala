package neat

import json._
import util.Globals._

class Species(var representant: Individual) {

	var individuals = List[Individual]()

	var fitnessSum = -1D
	var averageFitness = -1D


	def accepts(individual: Individual)(implicit args: EvolutionParameters) = representant.dist(individual) <= args.compatibilityThreshhold

	def add(individual: Individual): Unit = {
		individual.species = this
		individuals = individual :: individuals
	}

	def clear(): Unit = {
		representant = individuals(random.nextInt(individuals.size))
		individuals = List()
	}

	def calcAverageFitness(): Double = {
		fitnessSum = individuals.map(_.fitness).sum
		averageFitness = fitnessSum / individuals.size
		averageFitness
	}

	def roulletteWheel(): Individual = {
		var value = random.nextDouble() * fitnessSum
		individuals find { i =>
			value -= i.fitness
			value <= 0
		} getOrElse individuals.last
	}

	def produceOffspring(n: Int, tracker: InnovationTracker, speciesSelector: () => Species)(implicit args: EvolutionParameters, meta: ExperimentMeta): Seq[Individual] = {
		0 until n map {i =>
			if(i == 0 && n > 5) {
				new Individual(individuals maxBy (_.fitness))
			}
			else if(individuals.size == 1) {
				val mom = roulletteWheel()
				val baby = new Individual(mom)
				baby.mutate(tracker)
				baby
			}
			else {
				val mom = roulletteWheel()
				val dad = if(random.nextDouble() <= args.interspeciesMateRate) {
					speciesSelector().roulletteWheel()
				} else {
					roulletteWheel()
				}

				val baby = mom.mate(dad)
				baby.mutate(tracker)
				baby
			}
		}
	}
}
