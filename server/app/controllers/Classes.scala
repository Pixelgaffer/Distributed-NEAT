package controllers

import java.nio.file.Files

import com.redis.serialization.Parse
import play.api._
import play.api.cache._
import play.api.mvc._
import views._
import json._
import neat.NeatManager
import Parse.Implicits._
import javax.inject.Inject
import scala.concurrent.ExecutionContext.Implicits.global
import util._

import scala.concurrent.Future

class Classes @Inject() (cache: CacheApi) extends Controller {

	def update(name: String, platform: String) = Action.async(parse.multipartFormData) { request => Future {
		request.body.file("bin") foreach (f => updateApi(name, platform,  Files.readAllBytes(f.ref.file.toPath)))
		Ok("""{}""")
	}}

	def updateApi(name: String, platform: String, file: Array[Byte]) {
		cache.remove(name + ":" + platform)
		Redis.client.set(s"class:$name", " ")
		Redis.client.set(name + ":bin:" + platform, file)
	}


	def start(name: String) = Action.async(BodyParsers.parse.json) { request => Future {
		val experiment = request.body.validate[Experiment]
		experiment map { e =>
			if(e.args == null || e.inputNeurons <= 0 || e.outputNeurons <= 0)
				BadRequest("""{"error":"Invalid JSON"}""")
			else {
				Ok(s"{\"id\": ${startApi(name, e.inputNeurons, e.outputNeurons, e.args)}}")
			}
		} getOrElse {
			BadRequest("""{"error":"Invalid JSON"}""")
		}
	}}

	def startApi(name: String, inputNeurons: Int, outputNeurons: Int, args: EvolutionParameters): Int = NeatManager.startExperiment(args, inputNeurons, outputNeurons)


	def binary(name: String, platform: String) = Action.async { request => Future {
		binaryApi(name, platform) map { bin =>
			Ok(bin)
		} getOrElse {
			NotFound("""{"error":"Binaries not Found"}""")
		}
	}}

	def binaryApi(name: String, platform: String): Option[Array[Byte]] = cache.getOrElse(name + ":" + platform)(Redis.client.get[Array[Byte]](name + ":bin:" + platform))


}
