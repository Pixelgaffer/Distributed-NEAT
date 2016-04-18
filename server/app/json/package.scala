import play.api.libs.json._

package object json {

	case class EvolutionParameters(populationSize: Int = 100,
	                               excessCoefficient: Double = 2.0,
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
	                               weightMutPower: Double = 2.5)
	implicit val argumentFormat = Json.format[EvolutionParameters]

	case class Experiment(args: EvolutionParameters, inputNeurons: Int, outputNeurons: Int)
	implicit val experimentFormat = Json.format[Experiment]

}
