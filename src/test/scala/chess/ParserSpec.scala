package chess
import atto._,Atto._
import java.nio.file.Paths

import better.files.{File => BetterFile, _}
import chess.Parsers.Game
import org.scalatest._

import scala.io.Codec

/**
 * Created by ankit on 10/14/15.
 */
class ParserSpec extends FlatSpec with Matchers {

  "A PGN Parser" should "parse worldchampdb" in {
    val pgn = Paths.get(".") / "resources" / "worldchampdb.truncated.pgn"
    val gamesParser = Parsers.Game.games
    val parsed = gamesParser.parse(pgn.contentAsString(Codec.ISO8859)).done
    parsed.option shouldBe defined

    println(parsed)

  }




}
