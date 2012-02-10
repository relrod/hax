import org.jibble.pircbot._
import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.ql.basic.BasicDriver.Implicit._
import Karma._

object Hax {
  def main(args: Array[String]) {
    val db = Database.forURL("jdbc:sqlite:hax.sqlite", driver = "org.sqlite.JDBC")
    val bot: HaxBot = new HaxBot(nick = "Hax", database = db)
    bot.setVerbose(true)
    bot.setComChar(".")
    bot.connect("irc.tenthbit.net")
    bot.joinChannel("#bots")
  }
}

class HaxBot(nick: String, database: Database) extends PircBot {
  def setComChar(newComChar: String) = comChar = newComChar
  var comChar = """\."""
  setName(nick)

  try {
    database withSession {
      // Create the tables if they don't exist.
      Karma.ddl create
    }
  } catch {
    case sqle: java.sql.SQLException => println("*** SQLite3 table exists.")
  }
  
  override def onMessage(channel: String, sender: String, login: String, hostname: String, message: String) {
    val CommandWithArguments = ("^" + comChar + "(.+) (.+)").r
    val CommandWithoutArguments = ("^" + comChar + "(.+)").r
    val KarmaCommand = ("""^(.+)(--|\+\+)""").r
    
    message match {

      case KarmaCommand(item, karma) => {
        karma match {
          case "++" => sendMessage(channel, dispenseKarma(item, "up"))
          case "--" => sendMessage(channel, dispenseKarma(item, "down"))
        }
      }

      case CommandWithArguments(command, arguments) => {
        command match {
          case "hit" => sendAction(channel, "hits " + arguments + "with a ><>.")
          case _ =>
        }
      }

      case CommandWithoutArguments(command) => {
        command match {
          case "time" => {
            val time: String = (new java.util.Date).toString
            sendMessage(channel, sender + ": " + time)
          }
          case _ =>
        }
      }

    }
  }

  private def dispenseKarma(item_key: String, direction: String): String = {
    database withSession {

      // Check if the item exists already.
      var karma = for(k <- Karma if k.item === item_key) yield k.karma
      if (karma.list.isEmpty) {
        Karma.item ~ Karma.karma insert(item_key, 0)
        karma = for(k <- Karma if k.item === item_key) yield k.karma
      }

      direction match {
        case "up" => karma.update(karma.first + 1)
        case "down" => karma.update(karma.first - 1)
      }

      "Karma for \"" + item_key + "\" is now " + karma.first + "."    
    }
  }

}
