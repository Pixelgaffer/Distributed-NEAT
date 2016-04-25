package util

import json.ExperimentMeta
import neat.Individual

import scala.util.Random

object Globals {
	val random = new Random

	def stringify(population: Seq[Individual], simplify: Boolean): String = {
		val string = new StringBuilder

		string.append(s"${population.size}\n")
		population foreach { individual =>
			string.append(s"${individual.toString(false)}\n")
		}

		string.mkString
	}

	def loadPopulation(string: String, meta: ExperimentMeta): List[Individual] = {
		var lines = string.split("\n") filterNot (_.isEmpty)
		val size = lines(0).toInt
		lines = lines drop 1
		var result = List[Individual]()

		for(i <- 0 until size) {
			val iSize = lines(0).split(" ")(0).toInt
			val sub = lines take (iSize + 1)
			lines = lines.drop(iSize + 1)
			result = new Individual(sub.foldLeft("") ((acc, e) => acc + e + "\n"))(meta) :: result
		}

		result
	}
}
