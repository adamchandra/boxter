package acs.boxes


object Box extends App {

  override def main(args: Array[String]) = {
    val sample = """
    |/home/saunders/projects/the-livingroom/dvcs-mirrors/github.com/scalaz@github.com/scalaz-7/xml/src/main/scala/scalaz/xml/cursor/HCursor.scala:75:object HCursor extends HCursors {
    |/home/saunders/projects/the-livingroom/dvcs-mirrors/github.com/scalaz@github.com/scalaz-7/xml/src/main/scala/scalaz/xml/cursor/History.scala:4:sealed trait History {
    |/home/saunders/projects/the-livingroom/dvcs-mirrors/github.com/scalaz@github.com/scalaz-7/xml/src/main/scala/scalaz/xml/cursor/History.scala:23:trait Historys {
    |/home/saunders/projects/the-livingroom/dvcs-mirrors/github.com/scalaz@github.com/scalaz-7/xml/src/main/scala/scalaz/xml/cursor/History.scala:46:object History extends Historys
    |/home/saunders/projects/the-livingroom/dvcs-mirrors/github.com/scalaz@github.com/scalaz-7/xml/src/main/scala/scalaz/xml/cursor/Op.scala:7:sealed trait Op {
    |/home/saunders/projects/the-livingroom/dvcs-mirrors/github.com/scalaz@github.com/scalaz-7/xml/src/main/scala/scalaz/xml/cursor/Op.scala:197:private case clas
    """.trim.stripMargin.split("\n").toList.map(_.trim)

    /// val asdf  = sample.map(_.split(":", 3)).toList

    // println(asdf)

    
  }
}

