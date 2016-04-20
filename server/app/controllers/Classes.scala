package controllers

import java.nio.file.Files

import com.redis.serialization.Parse
import play.api._
import play.api.mvc._
import views._
import json._
import neat.NeatManager
import Parse.Implicits._

import play.api.libs.json.{JsValue, Json}

import scala.concurrent.ExecutionContext.Implicits.global
import util._

import scala.concurrent.Future

object Classes extends Controller {

	def update(name: String, platform: String) = Action.async(parse.multipartFormData) { request => Future {
		request.body.file("bin") foreach (f => updateApi(name, platform,  Files.readAllBytes(f.ref.file.toPath)))
		Ok("{}")
	}}

	def updateApi(name: String, platform: String, file: Array[Byte]) {
		Redis.useClient { client =>
			client.set(s"class:$name", " ")
			client.set(name + ":bin:" + platform, file)
		}
	}


	def start(name: String) = Action.async(BodyParsers.parse.json) { request => Future {
		val experiment = request.body.validate[Experiment]
		experiment map { e =>
			if(e.args == null || e.meta == null)
				BadRequest(new Error("Invalid JSON").prettyPrint)
			else {
				Ok(new StartedExperiment(NeatManager.startExperiment(name, e.args, e.meta)).prettyPrint)
			}
		} getOrElse {
			BadRequest(new Error("Invalid JSON").prettyPrint)
		}
	}}

	def startApi(name: String, args: EvolutionParameters, meta: ExperimentMeta): Int = NeatManager.startExperiment(name, args, meta)


	def binary(name: String, platform: String) = Action.async { request => Future {
		binaryApi(name, platform) map { bin =>
			Ok(bin)
		} getOrElse {
			NotFound(new Error("Binaries not found").prettyPrint)
		}
	}}

	def binaryApi(name: String, platform: String): Option[Array[Byte]] = Redis.useClient(_.get[Array[Byte]](name + ":bin:" + platform))


	def classes() = Action.async {request => Future {
		Ok(Json.prettyPrint(Json.toJson(classesApi().toArray)))
	}}

	def classesApi() = Redis.getKeys("class") sorted


	def classs(name: String) = Action {
		Ok(classApi(name).prettyPrint)
	}

	def classApi(name: String): Class = new Class(Redis.getKeys(name + ":bin") sorted, Redis.getKeys("experiments:" + name) filterNot (_.contains(":")) map (_.toInt) sorted)

}
