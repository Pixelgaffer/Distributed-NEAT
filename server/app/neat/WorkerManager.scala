package neat

import json.EvolutionParameters

import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable
import scala.concurrent.Future
import util.Globals._

object WorkerManager {

	val list: mutable.Buffer[Worker] = mutable.Buffer()

	def evalPopulation(population: List[Individual])(implicit args: EvolutionParameters) = Future[Unit] {
		//just for testing
		population foreach (i => i.fitness = ((i.genome filter (_.disabled) map (_.weight) sum) - (i.genome filterNot (_.disabled) map (_.weight) sum)).max(0))
	}

}
