package neat

import json.EvolutionParameters
import util.Globals._

case class Connection(from: Int, to: Int, var weight: Double, var disabled: Boolean, innovation: Int) {
	def this(connection: Connection) {
		this(connection.from, connection.to, connection.weight, connection.disabled, connection.innovation)
	}
}

class Individual(var genome: List[Connection], var species: Species) {

	var fitness: Double = -1

	var nodes = genome flatMap (c => List(c.to, c.from)) distinct

	def this(clone: Individual) {
		this(clone.genome.clone(), clone.species)
	}



	def dist(individual: Individual)(implicit args: EvolutionParameters): Double = {
		var weightDifference = 0D
		var matchingGenes = 0
		var excessGenes = 0
		var disjointGenes = 0

		var i = genome
		var j = individual.genome

		val higher = List(i, j).maxBy(_.head.innovation)
		val lower = List(i, j).minBy(_.head.innovation)
		while(higher.head.innovation > lower.head.innovation) {
			excessGenes += 1
		}

		while(i.nonEmpty && j.nonEmpty) {
			if(i.head.innovation > j.head.innovation) {
				i = i.tail
				disjointGenes += 1
			}
			if(i.head.innovation == j.head.innovation) {
				weightDifference += (i.head.weight - j.head.weight).abs
				matchingGenes += 1
				i = i.tail
				j = j.tail
			}
			if(j.head.innovation > i.head.innovation) {
				j = j.tail
				disjointGenes += 1
			}
		}

		while(i.nonEmpty) {
			excessGenes += 1
			i = i.tail
		}
		while(j.nonEmpty) {
			excessGenes += 1
			j = j.tail
		}

		var result = (excessGenes * args.excessCoefficient + disjointGenes * args.disjointCoefficient) / individual.genome.size.max(genome.size)
		if(matchingGenes > 0)
			result += weightDifference * args.weightDifferenceCoefficient / matchingGenes
		result
	}


	def mate(dad: Individual)(implicit args: EvolutionParameters): Individual = {
		val isBetterParent = if(this.fitness > dad.fitness) true else if(dad.fitness > this.fitness) false else if(random.nextBoolean()) true else false
		val betterParent = if(isBetterParent) this else dad
		var newGenome = List[Connection]()

		var i = genome
		var j = dad.genome

		while(i.nonEmpty && j.nonEmpty) {
			if(i.head.innovation > j.head.innovation) {
				if(isBetterParent) newGenome = new Connection(i.head) :: newGenome
				i = i.tail
			}
			else if(i.head.innovation == j.head.innovation) {
				newGenome = new Connection((if(random.nextDouble() < (if(isBetterParent) 0.7 else 0.3)) i else j).head) :: newGenome
				i = i.tail
				j = j.tail
			}
			else if(j.head.innovation > i.head.innovation) {
				if(!isBetterParent) newGenome = new Connection(j.head) :: newGenome
				j = j.tail
			}
		}

		if(isBetterParent) while(i.nonEmpty) {
			newGenome = new Connection(i.head) :: newGenome
			i = i.tail
		}
		if(!isBetterParent) while(j.nonEmpty) {
			newGenome = new Connection(j.head) :: newGenome
			j = j.tail
		}

		new Individual(newGenome.reverse, betterParent.species)
	}

	def mutate(tracker: InnovationTracker)(implicit args: EvolutionParameters): Unit = {
		if(random.nextDouble() < args.mutateAddNodeProb)
			mutateAddNode(tracker)
		else if(random.nextDouble() < args.mutateAddLinkProb)
			mutateAddLink(tracker)
		else {
			if(random.nextDouble() < args.mutateLinkWeightsProb)
				mutateLinkWeights()
			if(random.nextDouble() < args.mutateToggleEnableProb)
				mutateToggleEnable()
			if(random.nextDouble() < args.mutateToggleReenableProb)
				mutateToggleReenable()
		}
	}


	private def mutateAddNode(tracker: InnovationTracker): Unit = {
		var connection: Connection = null
		0 until 20 foreach { i =>
			if(connection != null) return
			val con = genome(random.nextInt(genome.size))
			if(!con.disabled)
				connection = con
		}
		if(connection == null) return

		connection.disabled = true
		val nodeInnovation = tracker.insertNodeInnovation(connection.from, connection.to)
		val firstInnovation = tracker.insertConnectionInnovation(connection.from, nodeInnovation)
		val secondInnovation = tracker.insertConnectionInnovation(nodeInnovation, connection.to)
		genome = new Connection(nodeInnovation, connection.to, connection.weight, false, secondInnovation) :: new Connection(connection.from, nodeInnovation, 1, false, firstInnovation) :: genome
		nodes = nodeInnovation :: nodes
	}

	private def mutateAddLink(tracker: InnovationTracker): Unit = {
		val nodeFrom = nodes(random.nextInt(nodes.size))
		val nodeTo = nodes(random.nextInt(nodes.size))

		val innovation = tracker.insertConnectionInnovation(nodeFrom, nodeTo)

		genome = new Connection(nodeFrom, nodeTo, random.nextDouble() * 2 - 1, true, innovation) :: genome
	}

	private def mutateLinkWeights()(implicit args: EvolutionParameters): Unit = {
		val severe = random.nextBoolean()

		genome foreach { connection =>
			var gausspoint: Double = 0
			var coldGausspoint: Double = 0

			if(severe) {
				gausspoint = 0.3
				coldGausspoint = 0.1
			}
			else if(random.nextBoolean()) {
				gausspoint = 0
				coldGausspoint = -0.1
			}
			else {
				gausspoint = 0
				coldGausspoint = 0
			}

			val rand = random.nextDouble() * args.weightMutPower * 2 - args.weightMutPower
			val randchoice = random.nextDouble()
			if(randchoice > gausspoint)
				connection.weight += rand
			else if(randchoice > coldGausspoint)
				connection.weight = rand
			connection.weight = connection.weight.max(-8).min(8)
		}
	}

	private def mutateToggleEnable(): Unit = {
		val randCon = genome(random.nextInt(genome.size))
		randCon.disabled = !randCon.disabled
	}

	private def mutateToggleReenable(): Unit = {
		val randCon = genome(random.nextInt(genome.size))
		randCon.disabled = true
	}

}
