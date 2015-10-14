package chess.models

/**
 * Created by ankit on 10/12/15.
 */
case class Pos(col: Char, row: Int) {
  override def toString = { col.toString + row }
}
