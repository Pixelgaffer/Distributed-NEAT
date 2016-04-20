package util

import com.redis.RedisClient

object Redis {

	private val client = new RedisClient("localhost", 6379)

	def useClient[E](consumer: RedisClient => E): E = client.synchronized {
		consumer(client)
	}

	def getKeys(startWith: String): List[String] = useClient { client => client.keys[String](s"$startWith:*") getOrElse List() map (_.getOrElse("")) filter (_.nonEmpty) map (_.substring(startWith.size + 1))}

}
