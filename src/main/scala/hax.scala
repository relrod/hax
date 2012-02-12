import org.jibble.pircbot._
import dispatch.{Http, HttpsLeniency, url}
import dispatch.jsoup.JSoupHttp._
import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.ql.basic.BasicDriver.Implicit._
import org.scalaquery.ql._
import java.sql.Timestamp
import java.util.Date
import Karma._
import Quote._

object Hax {
  def main(args: Array[String]) {
    val db = Database.forURL("jdbc:sqlite:hax.sqlite", driver = "org.sqlite.JDBC")
    val bot: HaxBot = new HaxBot(nick = "Hax", database = db)
    bot.setVerbose(true)
    bot.setComChar(".")
    bot.connect("irc.tenthbit.net")
    bot.joinChannel("#offtopic")
  }
}

class HaxBot(nick: String, database: Database) extends PircBot {
  def setComChar(newComChar: String) = comChar = newComChar
  var comChar = """\."""
  setName(nick)

  try {
    database withSession {
      // Create the tables if they don't exist.
      (Karma.ddl ++ Quote.ddl) create
    }
  } catch {
    case sqle: java.sql.SQLException => println("*** SQLite3 table exists.")
  }

  val CommandWithArguments = ("^" + comChar + "(.+) (.+)").r
  val CommandWithoutArguments = ("^" + comChar + "(.+)").r
  val KarmaCommand = ("""(?i)^([a-z0-9\.]+)(--|\+\+)""").r
  val URLRegex = ("""(?i).*?(https?://[\S]+) ?.*""").r 
  val TwitterRegex = """(?i).*?https?://twitter.com/.*/status(?:es|)/(\d+) ?.*""".r
  
  override def onMessage(channel: String, sender: String, login: String, hostname: String, message: String) {
    if (message == "`meep") {
      sendMessage(channel, "meep")
      return
    }

    message match {
      case TwitterRegex(tweetID) => sendMessage(channel, "\"" + fetchTweet(tweetID) + "\"")
      case URLRegex(fullURL) => sendMessage(channel, fetchURLTitle(fullURL))

      case KarmaCommand(item, karma) => {
        karma match {
          case "++" => sendMessage(channel, dispenseKarma(item, "up"))
          case "--" => sendMessage(channel, dispenseKarma(item, "down"))
        }
      }

      case CommandWithArguments(command, arguments) => {
        command match {
          case "hit" => sendAction(channel, "hits " + arguments + "with a ><>.")
          case "aquote" => {
            val quoteID = addQuote(arguments, sender, channel)
            sendMessage(channel, sender + ": I've added your quote, #" + quoteID + ".")
          }
          case _ =>
        }
      }

      case CommandWithoutArguments(command) => {
        command match {
          case "time" => {
            val time: String = (new java.util.Date).toString
            sendMessage(channel, sender + ": " + time)
          }
          case "rquote" => sendMessage(channel, sender + ": " + randomQuote())
          case _ =>
        }
      }
      case _ =>
    }
  }

  /** Fetch the title of a given URL.
   *
   * Fetch the contents of a <title>...</title> tag from a URL using
   * the Dispatch/JSoup set of libraries.
   */
  private def fetchURLTitle(theURL: String): String = {
    val https = new Http with HttpsLeniency
    val title = https(url(theURL) </> { html => html.title }).toString
    if (!title.isEmpty)
      "\"" + title.replace("\n", "") + "\""
    else
      ""
  }

  private def fetchTweet(tweetID: String): String = {
    val https = new Http with HttpsLeniency
    val twitterURL = "http://api.twitter.com/1/statuses/show.xml?id=" + tweetID
    https(url(twitterURL) </> { status => status.select("text").text }).toString
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

  /** Add a quote to the quote database. Return nothing.
   */
  private def addQuote(quote: String, nick: String, channel: String): Int = {
    database withSession {
      val timestamp = (new Date).getTime.toString
      Quote.quote ~ Quote.added_by ~ Quote.channel ~ Quote.timestamp insert(quote, nick, channel, timestamp)
      Query(Quote.count).first.toInt
    }
  }

  /** Pick a random Quote from the database and return it.
   *
   * This method is a bit inefficient, as rather than using ORDER BY RANDOM(),
   * (can ScalaQuery do that? TODO) it makes two queries, one to get the number
   * of quotes, then another to fetch a random one within that range.
   */
  private def randomQuote(): String = {
    database withSession {
      val randomQuoteID: Int = scala.util.Random.nextInt(Query(Quote.count).first + 1)
      val quote = for(q <- Quote if q.id === randomQuoteID) yield q.quote
      quote.first
    }
  }

}
