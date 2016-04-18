package neat

import json.EvolutionParameters

import scala.collection.mutable
import scala.concurrent.Future

object WorkerManager {

	val list: mutable.Buffer[Worker] = mutable.Buffer()

	def evalPopulation(population: List[Individual])(implicit args: EvolutionParameters) = Future {

		true
	}

}
