package me.elrod.hax
import scala.slick.driver.SQLiteDriver.simple._
import Database.threadLocalSession

/** Handle Hax command parsing. */
object Command {
  val IPRegex = """^(\d[\d\.]+\d)$""".r

  /**
   * Commands without arguments.
   *
   * @param command the command that was called
   * @return an Option[String] containing the response
   */
  def apply(command: String, db: Database): Option[String] = command match {
    case "time" => Some(new java.util.Date().toString)
    case "dbtest" => Some(db.toString)
    case "meme" => Some(io.Source.fromURL("http://api.automeme.net/text")
      .mkString
      .lines
      .toList
      .head)
    case _ => None
  }

  /**
   * Commands with arguments.
   *
   * @param command the command that was called
   * @param arguments a String containing arguments for the command
   * @return an Option[String] containing the response
   */
  def apply(command: String, arguments: String, db: Database): Option[String] = {
    command.drop(0) match {
      case "slap" | "fishify" =>
        Some("\001ACTION slaps %s with a ><>.\001".format(arguments))
      case "host" | "dns" => IPRegex findFirstIn arguments match {
        case Some(ip) => Some(java.net.InetAddress.getByName(ip).getHostName())
        case None => Some(java.net.InetAddress.getAllByName(arguments)
          .map(_.getHostAddress)
          .mkString(", "))
      }
      case _ => None
    }
  }
}
