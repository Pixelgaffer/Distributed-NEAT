package neat

import com.redis.serialization.Parse.Implicits._
import json._
import play.api.libs.json.Json
import util.Redis
import util.Globals._
import scala.concurrent.ExecutionContext.Implicits.global


import scala.collection.mutable.HashMap
import scala.concurrent.Future


object NeatManager {

	val loadedNEATs = HashMap[Int, NEAT]()


	def createNEAT(arguments: EvolutionParameters, meta: ExperimentMeta): (NEAT, Int) = {
		var id = 0
		Redis.useClient { client =>
			id = client.get[Int]("experiment-counter") getOrElse {
				client.set("experiment-counter", 0); 0
			}
			client.set("experiment-counter", id + 1)
		}

		val neat = new NEAT()(arguments, meta)
		neat.experiment = id
		loadedNEATs(id) = neat
		(neat, id)
	}

	def neatLoop(neat: NEAT) = {
		neat.running = true
		while(!neat.stop) {
			println("steping: " + neat.generation)
			neat.step()
			println("saving: " + neat.population.maxBy(_.fitness).fitness)
			Redis.useClient { client =>
				client.set(s"experiments:${neat.className}:${neat.experiment}:generations", neat.generation)
				client.set(s"experiments:${neat.className}:${neat.experiment}:generation:${neat.generation}", stringify(neat.population))
			}
		}
		neat.running = false
	}

	def loadFrom(name: String, id: Int, generation: Int, arguments: EvolutionParameters): NEAT = {
		//TODO
		_
	}

	def startExperiment(name: String, arguments: EvolutionParameters, meta: ExperimentMeta): Int = {
		val (neat, id) = createNEAT(arguments, meta)

		neat.className = name

		Future {
			for(individual <- 0 until meta.populationSize) {
				var genome = List[Connection]()
				for (i <- 0 until meta.inputNeurons; j <- 0 until meta.outputNeurons)
					genome = new Connection(i, meta.sensors + j, random.nextDouble() * 2 - 1, random.nextDouble() < 0.2, neat.tracker.insertConnectionInnovation(i, meta.sensors + j)) :: genome
				neat.population = new Individual(genome, null)(meta) :: neat.population
			}

			WorkerManager.evalPopulation(neat.population)(arguments)
			Redis.useClient { client =>
				client.set(s"experiments:$name:$id", " ")
				client.set(s"experiments:$name:$id:generations", 0)
				client.set(s"experiments:$name:$id:meta", Json.toJson(meta))
				client.set(s"experiments:$name:$id:generation:${neat.generation}", stringify(neat.population))
			}
			neatLoop(neat)
		} onFailure {
			case e => e.printStackTrace()
		}

		id
	}

}
