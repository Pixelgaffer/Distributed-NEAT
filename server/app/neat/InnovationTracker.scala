package neat

import scala.collection.mutable.HashMap

class InnovationTracker {

	var nodeCounter = 0
	var connectionCounter = 0

	var nodeMap = HashMap[(Int, Int), Int]()
	var connectionMap = HashMap[(Int, Int), Int]()

	def insertNodeInnovation(from: Int, to: Int) = nodeMap getOrElseUpdate((from, to), {nodeCounter += 1; nodeCounter})

	def insertConnectionInnovation(from: Int, to: Int) = connectionMap getOrElseUpdate((from, to), {connectionCounter += 1; connectionCounter})

	def clear(): Unit = {
		nodeMap.clear()
		connectionMap.clear()
	}

}
