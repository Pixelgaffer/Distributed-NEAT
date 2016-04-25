import play.api.libs.json._

package object json {

	implicit val evolutionParametersFormat = Json.format[EvolutionParameters]
	case class EvolutionParameters(excessCoefficient: Double = 2.0,
	                               disjointCoefficient: Double = 2.0,
	                               weightDifferenceCoefficient: Double = 1.0,
	                               var compatibilityThreshhold: Double = 6.0,
	                               compatibilityModifier: Double = 0.3,
	                               numSpeciesTarget: Int = 10,
	                               survivalThreshhold: Double = 0.2,
	                               interspeciesMateRate: Double = 0.05,
	                               mutateAddNodeProb: Double = 0.0025,
	                               mutateAddLinkProb: Double = 0.1,
	                               mutateLinkWeightsProb: Double = 0.9,
	                               mutateToggleEnableProb: Double = 0,
	                               mutateToggleReenableProb: Double = 0,
	                               weightMutPower: Double = 2.5) {
		def json = Json.toJson(this)
		def prettyPrint = Json.prettyPrint(json)
		def stringify = Json.stringify(json)
	}

	implicit val experimentMetaFormat = Json.format[ExperimentMeta]
	case class ExperimentMeta(populationSize: Int, inputNeurons: Int, hasBias: Boolean, hasRandom: Boolean, outputNeurons: Int, evaluationTpe: Int, evaluationSize: Int) {
		val sensors = inputNeurons + (if(hasBias) 1 else 0) + (if(hasRandom) 1 else 0)
		val specialNodes = sensors + outputNeurons
		def json = Json.toJson(this)
		def prettyPrint = Json.prettyPrint(json)
		def stringify = Json.stringify(json)
	}

	implicit val experimentFormat = Json.format[Experiment]
	case class Experiment(args: EvolutionParameters, meta: ExperimentMeta) {
		def json = Json.toJson(this)
		def prettyPrint = Json.prettyPrint(json)
		def stringify = Json.stringify(json)
	}

	implicit val errorFormat = Json.format[Error]
	case class Error(error: String) {
		def json = Json.toJson(this)
		def prettyPrint = Json.prettyPrint(json)
		def stringify = Json.stringify(json)
	}

	implicit val startedExperimentFormat = Json.format[StartedExperiment]
	case class StartedExperiment(id: Int) {
		def json = Json.toJson(this)
		def prettyPrint = Json.prettyPrint(json)
		def stringify = Json.stringify(json)
	}

	implicit val classFormat = Json.format[Class]
	case class Class(binaries: Seq[String], experiments: Seq[Int]) {
		def json = Json.toJson(this)
		def prettyPrint = Json.prettyPrint(json)
		def stringify = Json.stringify(json)
	}

	implicit val connectionFormat = Json.format[Connection]
	case class Connection(from: Int, to: Int, weight: Double, disabled: Boolean, innovation: Int) {
		def json = Json.toJson(this)
		def prettyPrint = Json.prettyPrint(json)
		def stringify = Json.stringify(json)
	}

	implicit val individualJsonFormat = Json.format[IndividualJson]
	case class IndividualJson(fitness: Double, connections: Seq[Connection]) {
		def json = Json.toJson(this)
		def prettyPrint = Json.prettyPrint(json)
		def stringify = Json.stringify(json)
	}

	implicit val generationFormat = Json.format[Generation]
	case class Generation(generation: Int, population: Seq[IndividualJson]) {
		def json = Json.toJson(this)
		def prettyPrint = Json.prettyPrint(json)
		def stringify = Json.stringify(json)
	}

	implicit val generationSimpleFormat = Json.format[GenerationSimple]
	case class GenerationSimple(generation: Int, bestIndividual: IndividualJson) {
		def json = Json.toJson(this)
		def prettyPrint = Json.prettyPrint(json)
		def stringify = Json.stringify(json)
	}

	implicit val experimentJsonFormat = Json.format[ExperimentJson]
	case class ExperimentJson(id: Int, className: String, meta: ExperimentMeta, generations: Seq[GenerationSimple]) {
		def json = Json.toJson(this)
		def prettyPrint = Json.prettyPrint(json)
		def stringify = Json.stringify(json)
	}

	implicit val experimentSimpleFormat = Json.format[ExperimentSimple]
	case class ExperimentSimple(id: Int, className: String) {
		def json = Json.toJson(this)
		def prettyPrint = Json.prettyPrint(json)
		def stringify = Json.stringify(json)
	}
}
