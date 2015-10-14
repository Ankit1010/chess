package chess.models

/**
 * Created by ankit on 10/14/15.
 */
sealed trait Outcome
case object WhiteWins extends Outcome
case object BlackWins extends Outcome
case object Draw extends Outcome
case object Unknown extends Outcome

object Outcome {
  def apply(s:String): Outcome = s match {
    case "1-0" => WhiteWins
    case "0-1" => BlackWins
    case "1/2-1/2" => Draw
    case _ => Unknown
  }
}
