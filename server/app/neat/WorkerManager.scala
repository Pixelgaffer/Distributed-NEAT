package neat

import java.util.UUID

import json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.{Buffer, HashMap}
import scala.concurrent.{Await, Future, Promise}
import util.Globals._
import scala.concurrent.duration._
import util.Redis


class Job(val uuid: UUID, val individuals: Seq[Individual], val platforms: Seq[String]) {
	val promise = Promise[Unit]()
}

object WorkerManager {

	val workers = Buffer[Worker]()
	val jobs = HashMap[UUID, Job]()

	def evalPopulation(population: List[Individual], name: String)(implicit args: EvolutionParameters, meta: ExperimentMeta) = Future[Unit] {
		val platforms = Redis.getKeys(name + ":bin")
		var promises: Seq[Job] = List[Job]()

		meta.evaluationTpe match {
			case 1 =>
				for(i <- population) {
					val job = new Job(UUID.randomUUID(), List(i), platforms)
					addJob(job)
					promises.+:(job)
				}
			case 2 =>
				for(i <- population; j <- population if i != j) {
					val job = new Job(UUID.randomUUID(), List(i, j), platforms)
					addJob(job)
					promises.+:(job)
				}
			case 3 =>
				for(i <- population.indices by meta.evaluationSize) {
					val job = new Job(UUID.randomUUID(), population slice (i, (i + meta.evaluationSize).max(population.size)), platforms)
					addJob(job)
					promises.+:(job)
				}
		}

		while(promises.nonEmpty) {
			promises = for(i <- promises) yield {
				Await.ready(i.promise.future, 90 days)
				i.promise.future.value getOrElse(None) match {
					case Some(_) =>
						null
					case None =>
						val job = new Job(UUID.randomUUID(), i.individuals, i.platforms)
						addJob(job)
						job
				}
			}
		}
	}

	def addJob(job: Job): Unit = workers filterNot (_.isOccupied()) find { w => job.platforms.contains(w.platform)} match {
		case Some(worker) =>
			worker.executeJob(Some(job))
		case None =>
			jobs += (job.uuid -> job)
	}

	def getNewWork(platform: String): Option[Job] = {
		val res = jobs find (_._2.platforms.contains(platform)) map (_._2)
		res foreach (jobs -= _.uuid)
		res
	}

}
