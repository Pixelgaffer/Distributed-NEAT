package neat

import com.redis.serialization.Parse.Implicits._
import json._
import util.Redis

import scala.collection.mutable.HashMap


object NeatManager {

	val loadedNEATs = HashMap[Int, NEAT]()


	def createNEAT(arguments: EvolutionParameters, inputNeurons: Int, outputNeurons: Int): (NEAT, Int) = {
		val id = Redis.client.get[Int]("experiment-counter") getOrElse {Redis.client.set("experiment-counter", 0); 0}
		Redis.client.set("experiment-counter", id + 1)
		val neat = new NEAT



		loadedNEATs(id) = neat
		(neat, id)
	}

	def startExperiment(arguments: EvolutionParameters, inputNeurons: Int, outputNeurons: Int): Int = {
		val (neat, id) = createNEAT(arguments, inputNeurons, outputNeurons)

		id
	}

}
