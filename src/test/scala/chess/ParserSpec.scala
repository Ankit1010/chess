package chess
import atto._,Atto._
import java.nio.file.Paths

import better.files.{File => BetterFile, _}
import chess.Parsers.Game
import org.scalatest._

/**
 * Created by ankit on 10/14/15.
 */
class ParserSpec extends FlatSpec with Matchers {

  val testGames = Cmds.ls(Paths.get(".") / "resources")


  "A PGN parser"  should "parse test games" in {
    val gamesParser = Parsers.Game.games
    val parsedGames = testGames.map{file => gamesParser.parse(file.lines.mkString("\n")).done.option }.toStream
    all(parsedGames) shouldBe defined

    for (game <- parsedGames) println(game)
  }


}
