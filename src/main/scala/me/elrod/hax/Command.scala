package me.elrod.hax
import scala.slick.driver.SQLiteDriver.simple._
import Database.threadLocalSession
import net.liftweb.json._
import scala.concurrent.ExecutionContext.Implicits.global
import org.joda.time.{DateTime, DateTimeZone}
import org.joda.time.format.DateTimeFormat

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
    message: IRCMessage): Unit = command match {
    case "time" => Command("time", "GMT", db, message)
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
    message: IRCMessage): Unit = command match {
    case "slap" | "fishify" =>
        message.bot.sendAction(
          message.channel,
          "slaps %s with a ><>.".format(arguments))
    case "time" => {
      try {
        val dt = new DateTime().withZone(DateTimeZone.forID(arguments))
        message.bot.sendMessage(
          message.channel,
          DateTimeFormat.fullDateTime.print(dt))
      } catch {
        case e: IllegalArgumentException => message.bot.sendMessage(
          message.channel,
          "The timezone you specified, '%s', is invalid.".format(arguments))
      }
    }
    case "google" => {
      val google = URLSnarfers.getFutureRawBodyForURL(
        "http://ajax.googleapis.com/ajax/services/search/web",
        Some(
          Map(
            "v" -> "1.0",
            "q" -> arguments)))
      google onSuccess {
        case response => {
          val firstResult = (parse(response) \ "responseData" \ "results")(0)
          val JString(url) = firstResult \ "url"
          val JString(title) = firstResult \ "titleNoFormatting"
          message.bot.sendMessage(
            message.channel,
            "%s - %s".format(url, title))
        }
      }
      google onFailure {
        case f => println(f)
      }
    }
    case "can" | "realcan" => {
      val url = command match {
        case "can" => "http://can.tenthbit.net/"
        case "realcan" => "http://calendaraboutnothing.com/"
      }

      val can = URLSnarfers.getFutureRawBodyForURL(
        url + "~" + arguments + ".json")

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

      can onFailure {
        case failure => message.bot.sendMessage(
          message.channel,
          "A failure occurred. Invalid username? (" + failure.getMessage + ")")
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
