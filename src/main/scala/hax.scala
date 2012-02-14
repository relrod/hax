import org.jibble.pircbot._
import dispatch.{thread, Http, HttpsLeniency, url}
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
    if (message == "`meep") {
      sendMessage(channel, "meep")
      return
    }

    message match {
      case TwitterRegex(tweetID) => sendMessage(channel, "\"" + fetchTweet(tweetID) + "\"")
      case URLRegex(fullURL) => sendMessage(channel, fetchURLTitle(fullURL))

      case KarmaCommand(item, karma) => {
        karma match {
          case "++" => dispenseKarma(item, "up")
          case "--" => dispenseKarma(item, "down")
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
              sendMessage(channel, sender + ": '" + item + "' has " + getKarma(item) + " karma."))
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

  /** Fetch the title of a given URL.
   *
   * Fetch the contents of a <title>...</title> tag from a URL using
   * the Dispatch/JSoup set of libraries.
   */
  private def fetchURLTitle(theURL: String): String = {
    val https = new Http with HttpsLeniency
    val title = https(url(theURL) </> { html => html.title }).toString
    if (!title.isEmpty)
      "\"" + title.replace("\n", "").replaceAll("""\s+""", " ") + "\""
    else
      ""
  }

  private def fetchTweet(tweetID: String): String = {
    val https = new Http with HttpsLeniency
    val twitterURL = "http://api.twitter.com/1/statuses/show.xml?id=" + tweetID
    https(url(twitterURL) </> { status => status.select("text").text }).toString
  }

  /** Dispense positive or negaitve karma to a given item.
   *
   * Returns a big fat cup of nothing.
   */
  private def dispenseKarma(item_key: String, direction: String) {
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
    }
  }

  /** Get the karma for a given item.
   *
   * Returns an integer of the karma for an item.
   */
  private def getKarma(item_key: String): Int = {
    database withSession {
      // Check if the item exists already.
      var karma = for(k <- Karma if k.item === item_key) yield k.karma
      if (karma.list.isEmpty) 0
      else karma.first
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

  /** Fetch the weather for a given city.
   *
   * Uses the Google Weather API.
   * Stolen from duckinator:
   * https://github.com/duckinator/scala-stuff/blob/master/weather/weather.scala
   */
  private def fetchWeather(location: String): String = {
    val https = new Http with HttpsLeniency
    val myUrl = "http://www.google.com/ig/api?weather=" + java.net.URLEncoder.encode(location, "UTF-8")
    
    val weather = https(url(myUrl) </> {
      xml => {
        if (xml.select("condition").isEmpty) {
          "Weather could not be retrieved."
        } else {
          "Weather for " +
          xml.select("city").attr("data") +
          ". Conditions: " +
          xml.select("condition").attr("data") +
          "; Temp: " +
          xml.select("temp_f").attr("data") +
          "F (" +
          xml.select("temp_c").attr("data") +
          "C). " +
          xml.select("humidity").attr("data") +
          ". " +
          xml.select("wind_condition").attr("data") +
          "."
        }
      }
    })
    weather.toString
  }
}
