package me.elrod.hax
import org.jibble.pircbot._
import com.typesafe.config._
import scala.collection.JavaConverters._
import scala.slick.driver.SQLiteDriver.simple._
import Database.threadLocalSession

object Hax extends App {
  val config = ConfigFactory.load()
  val bot = new Hax(config)
}

/**
 * A Hax bot. Version 2.
 *
 * @constructor Create an instance of Hax
 * @param config A Typesafe config object
 */
class Hax(config: Config) extends PircBot {
  setName(config.getString("bot.nick"))
  setLogin(config.getString("bot.nick"))
  setVerbose(config.getBoolean("bot.verbose"))
  connect(config.getString("bot.server"),
    config.getInt("bot.port"),
    config.getString("bot.password"))
  config.getStringList("bot.autojoin").asScala.foreach(joinChannel)
  val comchar = config.getString("bot.comchar")
  val db = Database.forURL(config.getString("bot.database.jdbc"),
    driver = config.getString("bot.database.driver"))

  /**
   * Gets called every time a message gets sent to the channel.
   *
   * This is where we handle things like responding to commands.
   *
   * @param channel the channel the message was sent to
   * @param sender the nickname of the person sending the message
   * @param login the ident of the person sending the message
   * @param hostname the hostname/cloak of the person sending the message
   * @param message the message sent to the channel
   */
  override def onMessage(channel: String,
    sender: String,
    login: String,
    hostname: String,
    message: String) {
    val ircMessage = IRCMessage(this, channel, sender, login, hostname, message)
    if (message startsWith comchar)
      message.split(" ", 2) match {
        case Array(command) =>
          Command(command.drop(comchar.length), db, ircMessage)
        case Array(command, arguments) =>
          Command(command.drop(comchar.length), arguments, db, ircMessage)
      }
    else
      message match {
        case URLSnarfers.URLRegex(url) =>
          URLSnarfers.fetchTitle(url, ircMessage)
        case _ =>
      }
  }
}
