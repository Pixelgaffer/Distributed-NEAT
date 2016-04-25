package neat

import java.util.UUID

import play.api.libs.iteratee._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import util._
import util.Globals._

import scala.util.Success


class Worker(out: Concurrent.Channel[String], val platform: String) {

	WorkerManager.workers += this

	private var currentJob: Job = null
	executeJob(WorkerManager.getNewWork(platform))

	def iteratee = Iteratee.foreach[String](receive _) map {s => disconnected(); ""}

	def receive(msg: String): Unit = {
		println("received: " + msg)

		if(currentJob == null) {
			println("ERROR: WORKER RESPONDED, EVEN THOUGH THERE IS NO JOB RUNNING!!!!!! THIS IS A BUG!!!!")
			return
		}

		var split = msg.split("|")

		if(split.length != currentJob.individuals.length + 1) {
			println("ERROR: THE WORKER RESPONDED WITH TOO MANY LINES!!!!!! THIS IS A BUG!!!!")
			currentJob.promise.failure(JobExecutionFailed("Worker responded with an illegal statement!!!"))
			return
		}

		val uuid = UUID.fromString(split(0))
		split = split.drop(1)

		for(i <- split.indices) {
			currentJob.individuals(i).fitness = split(i).toDouble
		}

		currentJob.promise.complete(Success())
		currentJob = null
		executeJob(WorkerManager.getNewWork(platform))
	}

	def isOccupied() = currentJob != null

	def executeJob(option: Option[Job]): Unit = option foreach { job =>
		currentJob = job
		out.push(job.uuid.toString)
		out.push(stringify(job.individuals, true))
	}

	def disconnected(): Unit = {
		println("disconnected")
		if(isOccupied()) currentJob.promise.failure(JobExecutionFailed("Worker disconnected"))
		WorkerManager.workers -= this
	}
}
