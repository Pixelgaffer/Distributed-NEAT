package neat

import play.api.libs.iteratee._
import play.api.libs.concurrent.Execution.Implicits.defaultContext

class Worker(out: Concurrent.Channel[String], val platform: String) {

	WorkerManager.list += this

	def iteratee = Iteratee.foreach[String](receive _) map {s => disconnected(); "disconnected"}

	def receive(msg: String) = {
		println("received: " + msg)
	}

	def disconnected(): Unit = {
		println("disconnected")
		WorkerManager.list -= this
	}
}
