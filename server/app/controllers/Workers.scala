package controllers

import play.api._
import play.api.mvc._
import views._
import neat._
import play.api.libs.iteratee._

object Workers extends Controller  {

	def socket(platform: String) = WebSocket.using[String] {request =>
		val (out, channel) = Concurrent.broadcast[String]

		val worker = new Worker(channel, platform)
		val in = worker.iteratee

		(in, out)
	}

}