package util

import com.redis.RedisClient

object Redis {

	val client = new RedisClient("localhost", 6379)

	def getKeys(startWith: String): List[String] = client.keys[String](s"$startWith:*") getOrElse List() map (_.getOrElse("")) filter (_.nonEmpty) map (_.substring(startWith.size))

}
