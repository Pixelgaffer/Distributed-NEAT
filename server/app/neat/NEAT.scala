package neat

import json._
import util.Globals._

import scala.concurrent.Await
import scala.concurrent.duration._

class NEAT()(implicit val args: EvolutionParameters, implicit val meta: ExperimentMeta) {

	var species = List[Species]()
	var population = List[Individual]()
	val tracker = new InnovationTracker


	var className: String = null
	var experiment: Int = -1

	var generation = 0
	var stop = false
	var running = false

	private def speciesRouletteWheel(sum: Double)(): Species = {
		var value = random.nextDouble() * sum
		species find { s =>
			value -= s.averageFitness
			value <= 0
		} getOrElse species.last
	}

	def step(): Unit = {
		generation += 1

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

		// Breeds the population
		val fitnessSum = species map (_.calcAverageFitness()) sum
		val distributed = (species map (_.averageFitness.*(population.size./(fitnessSum)).toInt) sum) toInt
		val bestSpecies = species maxBy (_.averageFitness)

		population = species flatMap { s =>
			var nOffspring = (s.averageFitness / fitnessSum * population.size).toInt
			if(s == bestSpecies)
				nOffspring += population.size - distributed
			s.produceOffspring(nOffspring, tracker, speciesRouletteWheel(fitnessSum))
		}

		species foreach (_.clear())
		tracker.clear()

		// Evaluates the population
		Await.ready(WorkerManager.evalPopulation(population), 10 days)
	}

}
