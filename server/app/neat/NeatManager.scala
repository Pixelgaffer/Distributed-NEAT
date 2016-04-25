package neat

import com.redis.serialization.Parse.Implicits._
import controllers.Experiments
import json._
import play.api.libs.json.Json
import util.Redis
import util.Globals._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.HashMap
import scala.concurrent.Future


object NeatManager {

	val loadedNEATs = HashMap[Int, NEAT]()


	def createNEAT(arguments: EvolutionParameters, meta: ExperimentMeta, defaultId: Int = -1): (NEAT, Int) = {
		var id = defaultId
		if (id < 0) {
			Redis.useClient { client =>
				id = client.get[Int]("experiment-counter") getOrElse {
					client.set("experiment-counter", 0);
					0
				}
				client.set("experiment-counter", id + 1)
			}
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
				client.set(s"experiments:${neat.className}:${neat.experiment}:generation:${neat.generation}", stringify(neat.population, false))
			}
		}
		neat.running = false
	}

	def loadFrom(name: String, id: Int, generation: Int, arguments: EvolutionParameters, fork: Boolean): NEAT = {
		val generationJson = Experiments.generationApi(name, id, generation).get
		val meta = Json.fromJson[ExperimentMeta](Json.parse(Redis.useClient(_.get[String](s"experiments:$name:$id:meta")).get)).get
		val (neat, newId) = createNEAT(arguments, meta, if(fork) -1 else id)

		neat.population = generationJson.population.map (i => {
			val res = new Individual(i.connections.map (c => new Connection(c.from, c.to, c.weight, c.disabled, c.innovation)).toList, null)(meta)
			res.fitness = i.fitness
			res
		}).toList

		if(fork) {
			Redis.useClient { client =>
				client.set(s"experiments:$name:$newId", " ")
				client.set(s"experiments:$name:$newId:generations", 0)
				client.set(s"experiments:$name:$newId:meta", Json.toJson(meta))
				client.set(s"experiments:$name:$newId:generation:${neat.generation}", stringify(neat.population, false))
			}
			neat.generation = 0
		}
		else {
			neat.generation = generation
		}

		Future {
			neatLoop(neat)
		}

		neat
	}

	def startExperiment(name: String, arguments: EvolutionParameters, meta: ExperimentMeta): Int = {
		val (neat, id) = createNEAT(arguments, meta, -1)

		neat.className = name

		Future {
			for(individual <- 0 until meta.populationSize) {
				var genome = List[Connection]()
				for (i <- 0 until meta.inputNeurons; j <- 0 until meta.outputNeurons)
					genome = new Connection(i, meta.sensors + j, random.nextDouble() * 2 - 1, random.nextDouble() < 0.2, neat.tracker.insertConnectionInnovation(i, meta.sensors + j)) :: genome
				neat.population = new Individual(genome, null)(meta) :: neat.population
			}

			WorkerManager.evalPopulation(neat.population)(arguments, meta)
			Redis.useClient { client =>
				client.set(s"experiments:$name:$id", " ")
				client.set(s"experiments:$name:$id:generations", 0)
				client.set(s"experiments:$name:$id:meta", Json.toJson(meta))
				client.set(s"experiments:$name:$id:generation:${neat.generation}", stringify(neat.population, false))
			}
			neatLoop(neat)
		} onFailure {
			case e => e.printStackTrace()
		}

		id
	}

}
