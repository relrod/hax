package me.elrod.hax
import scala.slick.driver.SQLiteDriver.simple._
import Database.threadLocalSession
import net.liftweb.json._
import scala.concurrent.ExecutionContext.Implicits.global

/** Handle Hax command parsing. */
object Command {
  val IPRegex = """^(\d[\d\.]+\d)$""".r

  /** Commands without arguments.
    *
    * @param command the command that was called
    * @return an Option[String] containing the response
    */
  def apply(command: String,
    db: Database,
    message: IRCMessage) = command match {
    case "dbtest" => message.bot.sendMessage(message.channel, db.toString)
    case "meme" => {
      val meme = URLSnarfers.getFutureRawBodyForURL(
        "http://api.automeme.net/text")
      meme onSuccess {
        case response =>
          message.bot.sendMessage(
            message.channel,
            response.lines.toList.head)
      }
    }
    case _ => None
  }

  /** Commands with arguments.
    *
    * @param command the command that was called
    * @param arguments a String containing arguments for the command
    * @return an Option[String] containing the response
    */
  def apply(command: String,
    arguments: String,
    db: Database,
    message: IRCMessage) = command match {
    case "slap" | "fishify" =>
        message.bot.sendAction(
          message.channel,
          "slaps %s with a ><>.".format(arguments))
    case "can" | "realcan" => {
      val url = command match {
        case "can" => "http://can.tenthbit.net/"
        case "realcan" => "http://calendaraboutnothing.com/"
      }

      val can = URLSnarfers.getFutureRawBodyForURL(
        url + "/~" + arguments + ".json")

      can onSuccess {
        case response => {
          val json = parse(response)
          message.bot.sendMessage(
            message.channel,
            "Current streak: %s, Longest streak: %s".format(
              (json \ "current_streak").values.toString,
              (json \ "longest_streak").values.toString))
        }
      }
    }
    case "host" | "dns" => IPRegex findFirstIn arguments match {
      case Some(ip) =>
        message.bot.sendMessage(
          message.channel,
          java.net.InetAddress.getByName(ip).getHostName())
      case None => message.bot.sendMessage(
        message.channel,
        java.net.InetAddress.getAllByName(arguments)
        .map(_.getHostAddress)
        .mkString(", "))
    }
    case _ => None
  }
}
