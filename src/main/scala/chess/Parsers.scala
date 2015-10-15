package chess

import chess.models._
import scalaz.Scalaz._
import scalaz._

object Parsers extends App {
  import atto._
  import Atto._
  implicit class SuperParser[+A](p:Parser[A]) {
    //one or zero matches of p
    def ?():Parser[Option[A]] = { opt(p) }
  }

  implicit class SuperEither[A](e: \/[A,A]) {
    def <>():A = if (e.isLeft) e.toEither.left.get else e.toEither.right.get
  }


  object Tokens {
    val str = bracket(char('"'),many(noneOf("\"")).map(_.mkString("")),char('"'))
    val int = Atto.int
    val dot = char('.')
    val star = char('*')
    val lbracket = char('[')
    val rbracket = char(']')
    val lparen = char('(')
    val rparen = char(')')
    val langle = char('<')
    val rangle = char('>')
    val nag = (char('$') |@| int)((c,i) => i.toInt)

    val symbol = for { h <- letter || digit; t <- many(letter || digit || Atto.oneOf("_+#=:-")) } yield {
        val tail = t.map(_.fold(_.fold(identity,identity),identity))
        val head = h.fold(identity,identity)
        val l = head :: tail
        l.mkString("")
      }

    val wspc = many(Atto.satisfy(_.isWhitespace))
  }



  object TagPair {
    import Tokens._
    val tagPair = squareBrackets(token(symbol) ~ token(str))
    val tagPairs = many(token(tagPair))
  }

  object Moves {
    import Tokens._
    val pieceChar = oneOf("PNBRQK")
    val colChar = oneOf(('a' to 'h').mkString)
    val rowNum = oneOf(('1' to '8').mkString).map(_ - '0')

    val annotation:Parser[String] =
      string("??") | string ("!!") | string ("?!") | string ("!?") | string ("?") | string ("!")
    val inlineComment: Parser[String] = char(';') ~> many(noneOf("\n")).map(_.mkString)
    val blockComment: Parser[String] = braces(many(noneOf("}")).map(_.mkString))
    val comment: Parser[String] = inlineComment | blockComment
    def variation(plyNo:Int): Parser[List[Ply]] = parens(many(ply(plyNo)))
    val nag: Parser[Int] = char('$') ~> int
    val gameResult: Parser[Outcome] = (string("1-0") | string("0-1") | string("1/2-1/2") | string("?")).map { Outcome(_) } named "outcome"


    //write parsers that match prefixes after the suffix parsers to match correctly
    def basicPlyParser(plyNo: Int): Parser[Ply] =
        (token(pieceChar) ~ colChar ~ rowNum ~ char('x') ~ colChar ~ rowNum).map { case (((((pc,fromCol),fromRow),_),col),row) => PlyR(plyNo, Some(pc), Pos(col,row), Some(fromCol), Some(fromRow), true)} |
        (token(pieceChar) ~ colChar ~ rowNum ~ colChar ~ rowNum).map { case ((((pc,fromCol),fromRow),col),row) => PlyR(plyNo, Some(pc), Pos(col,row), Some(fromCol), Some(fromRow), false)} |
        (token(pieceChar) ~ colChar ~ char('x') ~ colChar ~ rowNum).map { case ((((pc,fromCol),_),col),row) => PlyR(plyNo, Some(pc), Pos(col,row), Some(fromCol), None, true) } |
        (token(pieceChar) ~ char('x') ~ colChar ~ rowNum).map { case (((pc,_),col),row) => PlyR(plyNo, Some(pc), Pos(col,row), None, None, true)} |
        (colChar ~ char('x') ~ colChar ~ rowNum).map { case (((fromCol,_),col),row) => PlyR(plyNo, Some('P'), Pos(col,row), Some(fromCol), None, true)}  |
        (token(pieceChar) ~ rowNum ~ colChar ~ rowNum).map { case ((((pc,rowNum),col),row)) => PlyR(plyNo, Some(pc), Pos(col,row), None, Some(rowNum), false) } |
        (token(pieceChar) ~ colChar ~ colChar ~ rowNum).map { case ((((pc,fromCol),col),row)) => PlyR(plyNo, Some(pc), Pos(col,row), Some(fromCol), None, false) } |
        (token(pieceChar) ~ colChar ~ rowNum).map { case ((pc,col),row) => PlyR(plyNo, Some(pc), Pos(col,row), None, None,false)} |
        (colChar ~ rowNum).map { case (col,row) => PlyR(plyNo, Some('P'), Pos(col,row), None, None,false)} |
        string("...").map { _ => ContinuedPly(plyNo) } |
        string("O-O-O").map { _ => CastleQueenSide(plyNo) } |
        string("O-O").map { _ => CastleKingSide(plyNo)} named "ply"

    val promParser: Parser[Char] = char('=') ~> token(pieceChar) named "prom"
    val checkOrMateParser: Parser[(Boolean,Boolean)] = ((char('#') | char('+'))?).map { (c:Option[Char]) =>
      if (!c.isDefined) (true,true)
      else if (c.get == '#') (false,true)
      else (true,false)
    } named "checkormate"

    def ply(plyNo:Int) = for { p: Ply <- token(basicPlyParser(plyNo));
                          maybeProm <- promParser.?;
                          (check,mate) <- checkOrMateParser
                          comm <- comment.? } yield {
      p match {
        case pr: PlyR => pr.copy(promTo=maybeProm, check=check,checkMate=mate,comment=comm)
        case ck: CastleKingSide => ck.copy(promTo=maybeProm, check=check,checkMate=mate,comment=comm)
        case cq: CastleQueenSide => cq.copy(promTo=maybeProm, check=check,checkMate=mate,comment=comm)
        case cp: ContinuedPly => cp.copy(comment=comm)
        case ep: EmptyPly => ep.copy(comment=comm)
      }
    }

    val moveNum: Parser[Int] = int <~ token(many(dot))
    val move: Parser[(Ply,Ply)] = for { moveNo <- moveNum;
                                        ply1 <- ply(2*moveNo-1);
                                        _ <- skipWhitespace;
                                        ply2 <- (ply(2*moveNo)?) } yield {
      (ply1,ply2.getOrElse(EmptyPly(2*moveNo, None)))
    }

    val moves: Parser[List[(Ply,Ply)]] = sepBy(token(move),wspc)
  }

  object Game {
    import Moves._
    import TagPair._
    import Tokens._
    val game = (token(tagPairs) |@| token(comment?) |@| token(moves) |@| token(gameResult)){case x => x}
    val games = sepBy(game, wspc)

  }
  
}
