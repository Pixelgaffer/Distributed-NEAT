package neat

import json.EvolutionParameters
import util.Globals._

import scala.concurrent.Await
import scala.concurrent.duration._

class NEAT(implicit val args: EvolutionParameters) {

	var species = List[Species]()
	var population = List[Individual]()

	var generation = 0

	val tracker = new InnovationTracker

	private def speciesRouletteWheel(sum: Double)(): Species = {
		var value = random.nextDouble() * sum
		species find { s =>
			value -= s.averageFitness
			value <= 0
		} getOrElse species.last
	}

	def step(): Unit = {
		generation += 1

		// Evaluates the population
		val evaluated = WorkerManager.evalPopulation(population)

		// Speciates the population
		population foreach { individual =>
			if(individual.species != null && individual.species.accepts(individual)) {
				individual.species.add(individual)
			}
			else {
				(species find (s => s != individual.species && s.accepts(individual)) getOrElse {
					val newSpecies = new Species(individual)
					species = newSpecies :: species
					newSpecies
				}).add(individual)
			}
		}

		species = species filter (_.individuals.nonEmpty)

		if(species.size > args.numSpeciesTarget) {
			args.compatibilityThreshhold += args.compatibilityModifier
		}
		else if(species.size < args.numSpeciesTarget) {
			args.compatibilityThreshhold -= args.compatibilityModifier
			args.compatibilityThreshhold = args.compatibilityThreshhold.max(args.compatibilityModifier)
		}

		// Waits for the evaluation to have finished
		Await.ready(evaluated, 0 nanos)

		// Breeds the population
		val fitnessSum = species map (_.calcAverageFitness()) sum
		val distributed = (species map[Int] (_.averageFitness / fitnessSum * population.size) sum) toInt
		val bestSpecies = species maxBy (_.averageFitness)

		population = species flatMap { s =>
			var nOffspring = (s.averageFitness / fitnessSum * population.size).toInt
			if(s == bestSpecies)
				nOffspring += population.size - distributed
			s.produceOffspring(nOffspring, tracker, speciesRouletteWheel(fitnessSum))
		}

		// TODO: save generation

		species foreach (_.clear())
		tracker.clear()
	}

}
