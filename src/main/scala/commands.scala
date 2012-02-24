package me.elrod.hax.commands
import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.ql.basic.BasicDriver.Implicit._
import org.scalaquery.ql._
import java.sql.Timestamp
import java.util.Date
import scalaj.http.{Http,HttpOptions}
import org.jsoup.Jsoup
import me.elrod.hax.tables._

// Functions used for commands go here.
object Command {
  // This is duplicated code (== bad). @todo
  val database = Database.forURL("jdbc:sqlite:hax.sqlite", driver = "org.sqlite.JDBC")

  /** Return the weather for a given location as a string.
    *
    * Use the Google Weather API to obtain weather for a location.
    * The given location is encoded with java.net.URLEncoder.encode(), using UTF-8.
    *
    * @param location the location to fetch weather for (zipcode or city/state/country name)
    */
  def fetchWeather(location: String): String = {
    try {
      val http = Http("https://www.google.com/ig/api?weather=" + java.net.URLEncoder.encode(location, "UTF-8")).option(HttpOptions.connTimeout(2000)).option(HttpOptions.readTimeout(2000))
      val document = Jsoup.parse(http.asString)
      if (document.select("problem_cause").isEmpty) {
        "Weather for " +
        getCurrent("city", document, parent = "forecast_information") + 
        ". Conditions: " +
        getCurrent("condition", document) +
        "; Temp: " +
        getCurrent("temp_f", document) +
        "F (" +
        getCurrent("temp_c", document) +
        "C). " +
        getCurrent("humidity", document) +
        ". " +
        getCurrent("wind_condition", document) + "."
      } else {
        "Couldn't get the weather for " + location
      }
    } catch {
      case e: java.net.SocketTimeoutException => "<timeout>"
      case _ => "<error>"
    }
  }

  /** Provides a way for fetchWeather() above to easily get current conditions from the Jsoup Document
   *
   * @param condition the condition to retrieve from <current_conditions>
   * @param document the Jsoup document to parse it from.
   */
  private def getCurrent(condition: String, parent: String = "current_conditions"): String = {
    document.select(parent + " " + condition,).attr("data")
  }

  /** Return a random quote from the global Quote Database
    *
    * This method is a tad inefficient. Rather than using ORDER BY RANDOM(),
    * it makes two queries - one to get the number of possible quotes,
    * then one to fetch a random one within 1 and that range.
    *
    * @todo see if scalaquery can do ORDER BY RANDOM().
    */

  def randomQuote(): String = {
    database withSession {
      val randomQuoteID: Int = scala.util.Random.nextInt(Query(Quote.count).first + 1)
      val quote = for(q <- Quote if q.id === randomQuoteID) yield q.id ~ q.quote
      quote.first match {
        case(id, quote) => { "(" + id + ") " + quote }
      }
    }
  }

  /** Add a quote to the quote database.
    *
    * @return the ID of the quote in the database.
    */
  def addQuote(quote: String, nick: String, channel: String): Int = {
    database withSession {
      val timestamp = (new Date).getTime.toString
      Quote.quote ~ Quote.added_by ~ Quote.channel ~ Quote.timestamp insert(quote, nick, channel, timestamp)
      Query(Quote.count).first.toInt
    }
  }

  /** Returns the karma (integer) for a given item.
    *
    * @param item_key the item to obtain karma for
    */
  def getKarma(item_key: String): Int = {
    database withSession {
      // Check if the item exists already.
      var karma = for(k <- Karma if k.item === item_key) yield k.karma
      if (karma.list.isEmpty) 0
      else karma.first
    }
  }

  /** Dispense positive or negaitve karma to a given item.
    *
    * @param item_key the item to increment karma for
    * @param direction the way to alter karma (a string "up" or "down")
    */
  def dispenseKarma(item_key: String, direction: String) {
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
}
