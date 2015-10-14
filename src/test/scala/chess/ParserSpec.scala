package chess

import java.nio.file.Paths

import better.files.{File => BetterFile, _}
import org.scalatest._

/**
 * Created by ankit on 10/14/15.
 */
class ParserSpec extends FlatSpec with Matchers {

  val testGames = Cmds.ls(Paths.get(".") / "resources").map(file => (file.name,file.lines))

  behavior of "PGN Parser"

  it should "parse test games" in {
    for ((fname,linesIt) <- testGames) {
      val lines = linesIt.mkString("\n")
      val parsed = Parsers.Game.game.parse(lines).done
      val success = parsed.option.map(_ => "SUCCESS!!!!!").getOrElse("FAIL!")
      println(s"Parsing $fname: $success $parsed")
      parsed.option.isDefined shouldBe true
    }
  }


}
