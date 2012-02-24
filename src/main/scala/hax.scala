import org.jibble.pircbot._
import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.ql.basic.BasicDriver.Implicit._
import org.scalaquery.ql._
import java.sql.Timestamp
import java.util.Date
import me.elrod.hax.tables._
import me.elrod.hax.urlSnarfing.urlSnarfing._
import me.elrod.hax.commands.Command._

object Hax {
  def main(args: Array[String]) {
    val db = Database.forURL("jdbc:sqlite:hax.sqlite", driver = "org.sqlite.JDBC")
    val bot: HaxBot = new HaxBot(nick = "Hax", database = db, ignoreNicks = List("yurbnurb"))
    bot.setVerbose(true)
    bot.connect("irc.tenthbit.net")
    bot.joinChannel("#offtopic")
  }
}

class HaxBot(nick: String, database: Database, comChar: String = "\\.", ignoreNicks: List[String] = List()) extends PircBot {
  setName(nick)
  setLogin(nick)

  try {
    database withSession {
      // Create the tables if they don't exist.
      (Karma.ddl ++ Quote.ddl) create
    }
  } catch {
    case sqle: java.sql.SQLException => println("*** SQLite3 table exists.")
  }

  val CommandWithArguments = ("^" + comChar + "(.+?) (.+)").r
  val CommandWithoutArguments = ("^" + comChar + "(.+)").r
  val KarmaCommand = ("""(?i)^(\S+)(--|\+\+)""").r
  val URLRegex = ("""(?i).*?(https?://[\S]+) ?.*""").r 
  val TwitterRegex = """(?i).*?https?://twitter.com/.*/status(?:es|)/(\d+) ?.*""".r
  
  override def onMessage(channel: String, sender: String, login: String, hostname: String, message: String) {
    if (ignoreNicks.contains(sender)) return

    if (message == "`meep") {
      sendMessage(channel, "meep")
      return
    }

    message match {
      case TwitterRegex(tweetID) => sendMessage(channel, "\"" + fetchTweet(tweetID) + "\"")
      case URLRegex(fullURL) => sendMessage(channel, fetchURLTitle(fullURL))

      case KarmaCommand(item, karma) => {
        karma match {
          case "++" => dispenseKarma(item.toLowerCase, "up")
          case "--" => dispenseKarma(item.toLowerCase, "down")
        }
      }

      case CommandWithArguments(command, raw_arguments) => {
        val arguments: String = raw_arguments.replaceAll("\\s+$", "")
        
        command match {
          case "hit" => sendAction(channel, "hits " + arguments + "with a ><>.")
          case "aquote" => {
            val quoteID = addQuote(arguments, sender, channel)
            sendMessage(channel, sender + ": I've added your quote, #" + quoteID + ".")
          }
          case "weather" => sendMessage(channel, sender + ": " + fetchWeather(arguments))
          case "karma" =>
            arguments.split(",").take(3).foreach(item =>
              sendMessage(channel, sender + ": '" + item.toLowerCase + "' has " + getKarma(item.toLowerCase) + " karma."))
          case _ =>
        }
      }

      case CommandWithoutArguments(command) => {
        command match {
          case "time" => {
            val time: String = (new java.util.Date).toString
            sendMessage(channel, sender + ": " + time)
          }
          case "rquote" => sendMessage(channel, randomQuote())
          case _ =>
        }
      }
      case _ =>
    }
  }
}
