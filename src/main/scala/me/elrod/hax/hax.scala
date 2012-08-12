import org.jibble.pircbot._
import com.typesafe.config._
import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.ql.basic.BasicDriver.Implicit._
import org.scalaquery.ql._
import me.elrod.hax.tables._
import me.elrod.hax.urlSnarfing.urlSnarfing._
import me.elrod.hax.commands.Command._

object Hax {
    System.setProperty("http.agent", "Mozilla/5.0 (X11; Linux x86_64; rv:13.0) Gecko/20120222 Firefox/13.0a1")

  def main(args: Array[String]) {
    val db = Database.forURL("jdbc:sqlite:hax.sqlite", driver = "org.sqlite.JDBC")
    val config = ConfigFactory.load()
    val bot: HaxBot = new HaxBot(nick = config.getString("bot.nick"), database = db, ignoreNicks = config.getList("bot.ignores").unwrapped.toArray)
    bot.setVerbose(true)
    bot.connect(config.getString("bot.network"))
    
    // Try authenticating, but if the config keys aren't there, just go on, don't die.
    try {
      bot.sendMessage(config.getString("bot.authenticate.to"), config.getString("bot.authenticate.with"))
    } catch {
      case e =>
    }
    
    config.getList("bot.autojoin").unwrapped.toArray.foreach(channel => bot.joinChannel(channel.toString))
  }
}

class HaxBot(nick: String, database: Database, comChar: String = "\\.", ignoreNicks: Array[java.lang.Object] = Array()) extends PircBot {
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
  val KarmaCommand = ("""(?i)^(\S+)(--|\+\+).*""").r
  val URLRegex = ("""(?i).*?(https?://\S+).*""").r 
  val TwitterRegex = """(?i).*?https?://twitter.com/.*/status(?:es|)/(\d+).*""".r
  val SpotifyRegex = """(?i).*spotify:(\w+):(\w+).*""".r
  val IPRegex = """^(\d[\d\.]+\d)$""".r
  val WikipediaRegex = """.*\[\[([^\[\]]+)\]\].*""".r

  // Thanks @duckinator for the regex.
  val YouTubeRegex = """(?i).*https?://(?:www\.)?youtu(?:\.be/|be\.com/watch\?(?:.+&)?v=)([^&#\s]+).*""".r
  
  override def onMessage(channel: String, sender: String, login: String, hostname: String, message: String) {
    if (ignoreNicks.contains(sender) || message.startsWith("^")) return

    message match {
      case "`meep" => sendMessage(channel, "meep")
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
          case "host" => {
            sendMessage(channel, arguments match {
              case IPRegex(ip) => java.net.InetAddress.getByName(ip).getHostName()
              case unknown => java.net.InetAddress.getAllByName(arguments).map(_.getHostAddress).mkString(", ")
            })
          }
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

      case SpotifyRegex(mediaType, identifier) =>
        sendMessage(channel, spotifyInfo(mediaType, identifier))
      //case WikipediaRegex(articleName) =>
      //  sendMessage(channel, "https://en.wikipedia.org/wiki/" + java.net.URLEncoder.encode(articleName, "UTF-8").replace("+", "%20"))


      case _ =>
    }
  }

  override def onAction(sender: String, login: String, hostname: String, target: String, action: String) =
    onMessage(target, sender, login, hostname, action)

  override def onPrivateMessage(sender: String, login: String, hostname: String, message: String) =
    onMessage(sender, sender, login, hostname, message)
}
