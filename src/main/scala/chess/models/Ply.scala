package chess.models

/**
 * Created by ankit on 10/14/15.
 */
sealed trait Ply {}

//normal ply
class PlyR(plyNo: Int,piece: Option[Char], dest: Pos,
                fromCol: Option[Char], fromRow: Option[Int],
                capture: Boolean, promTo: Option[Char] = None,
                check: Boolean = false,checkMate: Boolean = false,
                comment:Option[String] = None) extends Ply {
  override def toString = {
    val plyNoString = plyNo.toString
    val pieceString = piece.map(_.toString).getOrElse("")
    val colString =  fromCol.getOrElse("")
    val captureString = if (capture) "x" else ""
    val destString = dest

    plyNoString + ". " + pieceString + colString + captureString + destString
  }

  def copy(plyNo: Int = this.plyNo, piece: Option[Char] = this.piece, dest: Pos = this.dest,
           fromCol: Option[Char] = this.fromCol, fromRow: Option[Int] = this.fromRow,
           capture: Boolean = this.capture, promTo: Option[Char] = this.promTo,
           check: Boolean = this.check, checkMate: Boolean = this.checkMate,
           comment: Option[String] = None): Ply = {
    PlyR(plyNo,piece,dest,fromCol,fromRow,capture,promTo,check,checkMate, comment)
  }
}

object PlyR {
  def apply(plyNo: Int, piece: Option[Char], dest: Pos,
            fromCol: Option[Char], fromRow: Option[Int],
            capture: Boolean, promTo: Option[Char] = None,
            check: Boolean = false,checkMate: Boolean = false,
            comment: Option[String] = None) = {
    new PlyR(plyNo,piece,dest,fromCol,fromRow,capture,promTo,check,checkMate, comment)
  }

  def unapply(p: Ply) = p match {
    case x: PlyR => Some(x)
    case _ => None
  }
}

//O-O
class CastleKingSide(plyNo: Int, promTo: Option[Char] = None, check: Boolean = false, checkMate: Boolean = false,
                     comment:Option[String] = None) extends Ply {
  def copy(plyNo: Int = this.plyNo, promTo: Option[Char] = this.promTo,
           check: Boolean = this.check, checkMate: Boolean = this.checkMate,
           comment:Option[String] = None): CastleKingSide = {
    CastleKingSide(plyNo,promTo,check,checkMate,comment)
  }

  override def toString = { "O-O" }
}

object CastleKingSide {
  def apply(plyNo: Int, promTo: Option[Char] = None, check: Boolean = false, checkMate: Boolean = false,
            comment:Option[String] = None) = {
    new CastleKingSide(plyNo,promTo,check,checkMate,comment)
  }
}

//O-O-O
class CastleQueenSide(plyNo: Int,promTo: Option[Char] = None, check: Boolean = false, checkMate: Boolean = false,
                      comment:Option[String] = None) extends Ply {
  def copy(plyNo: Int = this.plyNo, promTo: Option[Char] = this.promTo,
           check: Boolean = this.check, checkMate: Boolean = this.checkMate,
           comment:Option[String] = None): CastleQueenSide = {
    CastleQueenSide(plyNo,promTo,check,checkMate, comment)
  }

  override def toString = { "O-O-O" }
}

object CastleQueenSide {
  def apply(plyNo: Int, promTo: Option[Char] = None, check: Boolean = false, checkMate: Boolean = false,
            comment:Option[String] = None) = {
    new CastleQueenSide(plyNo,promTo,check,checkMate, comment)
  }
}

//...
class ContinuedPly(plyNo: Int, comment:Option[String] = None) extends Ply {
  def copy(plyNo: Int = this.plyNo, comment:Option[String] = None) = ContinuedPly(plyNo,comment)
  override def toString = { "..." }
}

object  ContinuedPly {
  def apply(plyNo:Int,comment:Option[String] = None) = new ContinuedPly(plyNo,comment)
}

//empty, when game is interrupted or resigned
class EmptyPly(plyNo: Int, comment:Option[String] = None) extends Ply {
  def copy(plyNo: Int = this.plyNo, comment:Option[String] = None) = EmptyPly(plyNo,comment)
  override def toString = { "<>" }
}

object  EmptyPly {
  def apply(plyNo:Int,comment:Option[String] = None) = new EmptyPly(plyNo,comment)
}


