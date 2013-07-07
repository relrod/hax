package me.elrod.hax
import scala.slick.driver.SQLiteDriver.simple._
import Database.threadLocalSession
import net.liftweb.json._
import scala.concurrent.ExecutionContext.Implicits.global
import org.joda.time.{DateTime, DateTimeZone}
import org.joda.time.format.DateTimeFormat

import me.elrod.pkgwat._

/** Handle Hax command parsing. */
object Command {
  implicit val formats = net.liftweb.json.DefaultFormats
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
    case "pkgwat" => {
      val pkgwat = new Pkgwat
      pkgwat.releases(arguments) onSuccess { case results =>
        if (results.rows.length == 0) {
          message.bot.sendMessage(message.channel, "No results.")
        } else {
          val versions = results.rows.map { row =>
            s"${row.release}: s: ${row.stableVersion} t: ${row.testingVersion}"
          }
          message.bot.sendMessage(
            message.channel,
            versions.mkString(", "))
        }
      }
    }
    case "host" | "dns" => IPRegex findFirstIn arguments match {
      case Some(ip) => {
        try {
          message.bot.sendMessage(
            message.channel,
            java.net.InetAddress.getByName(ip).getHostName())
        } catch {
          case e: java.net.UnknownHostException => message.bot.sendMessage(
            message.channel,
            e.getMessage)
        }
      }
      case None => {
        try {
          message.bot.sendMessage(
            message.channel,
            java.net.InetAddress.getAllByName(arguments)
            .map(_.getHostAddress)
            .mkString(", "))
        } catch {
          case e: java.net.UnknownHostException => message.bot.sendMessage(
            message.channel,
            e.getMessage)
        }
      }
    }
    case _ => None
  }
}
