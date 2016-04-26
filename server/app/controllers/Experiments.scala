package controllers

import play.api.libs.json.Json
import play.api.mvc._
import util.Redis
import json._
import neat.NeatManager
import util.Globals._

object Experiments extends Controller {

	def continue(id: Int) = Action {
		//TODO
		Ok(" ")
	}

	def continueApi(id: Int): Boolean = {
		//TODO
		NeatManager.loadedNEATs.get(id) match {
			case Some(neat) => if(!neat.running) Some(neat) else None
			case None => Some(NeatManager.loadFrom(classFromId(id), id, ))
		}
		true
	}


	def stop(id: Int) = Action {
		if(stopApi(id))
			Ok("{}")
		else
			BadRequest(new Error("There is no experiment running with that ID!").prettyPrint)
	}

	def stopApi(id: Int): Boolean = {
		NeatManager.loadedNEATs.get(id) match {
			case Some(neat) =>  {
				val res = neat.stop
				neat.stop = true
				!res
			}
			case None => false
		}
	}


	def fork(id: Int, generation: Int) = Action {
		//TODO
		Ok(" ")
	}


	def generation(id: Int, generation:Int) = Action {
		classFromId(id) match {
			case Some(c) => Ok(generationApi(c, id, generation).get.prettyPrint)
			case None => BadRequest(new Error("ID could not be found!"))
		}
	}

	def generationApi(name: String, id: Int, generation: Int): Option[Generation] = {
		Redis.useClient(_.get[String](s"experiments:$name:$id:meta")) map { metaJson =>
			val meta = Json.fromJson[ExperimentMeta](Json.parse(metaJson)).get
			val string = Redis.useClient(_.get[String](s"experiments:$name:$id:generation:$generation")).get
			new Generation(generation, loadPopulation(string, meta) map (i => new IndividualJson(i.fitness, i.genome map(c => new Connection(c.from, c.to, c.weight, c.disabled, c.innovation)))) sortBy(_.fitness))
		}
	}


	def experiment(id: Int) = Action {
		classFromId(id) match {
			case Some(c) => Ok(experimentApi(c, id).prettyPrint)
			case None => BadRequest(new Error("ID could not be found!").prettyPrint)
		}
	}

	def experimentApi(name: String, id: Int): ExperimentJson = {
		val meta = Json.fromJson[ExperimentMeta](Json.parse(Redis.useClient(_.get[String](s"experiments:$name:$id:meta")).get)).get
		var generations = List[GenerationSimple]()

		Redis.getKeys(s"experiments:$name:$id:generation")  foreach { s =>
			val population = loadPopulation(Redis.useClient(_.get[String](s"experiments:$name:$id:generation:$s")).get, meta)
			val bestIndividual = population maxBy (_.fitness)
			generations = new GenerationSimple(s.toInt, new IndividualJson(bestIndividual.fitness, bestIndividual.genome map (c => new Connection(c.from, c.to, c.weight, c.disabled, c.innovation)))) :: generations
		}

		new ExperimentJson(id, name, meta, generations sortBy(_.generation))
	}


	def experiments() = Action {
		Ok(Json.prettyPrint(Json.toJson(experimentsApi())))
	}

	def experimentsApi() = Classes.classesApi() flatMap (c => Classes.classApi(c).experiments map (new ExperimentSimple(_, c))) sortBy(_.id)



	def classFromId(id: Int) = experimentsApi() find (_.id == id) map (_.className)
}
