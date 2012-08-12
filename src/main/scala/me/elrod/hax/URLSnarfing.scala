package me.elrod.hax
import scala.concurrent.{ Future, future }
import scala.concurrent.ExecutionContext.Implicits.global
import org.jsoup.Jsoup

object URLSnarfers {
  val URLRegex = """(?i).*?(https?://\S+).*""".r

  /** Concurrently get a JSoup Document with the data from the URL.
    *
    * This uses JSoup to go out and get the content of a URL, then calls get()
    * on it and returns the result as a Scala (2.10.0+) Future
    *
    * @param theURL the URL to be fetched
    * @return a future that, on success, will contain a
    *         [[org.jsoup.nodes.Document]] with the response.
    */
  def getFutureForURL(theURL: String) = {
    future {
      Jsoup.connect(theURL)
        .header("Accept-Language", "en-us,en;q=0.5")
        .header("Accept-Encoding", "gzip, deflate")
        .get()
    }
  }

  /** Concurrently get a String containing a raw HTTP response.
    *
    * This uses JSoup to go out and get the ''raw'' content of a URL.
    *
    * @param theURL the URL to get the ''raw'' content from
    * @return a future that, on success, will contain a String containing the
    *         ''raw'' response from the server.
    */
  def getFutureRawBodyForURL(theURL: String) = {
    future {
      Jsoup.connect(theURL)
        .header("Accept-Language", "en-us,en;q=0.5")
        .header("Accept-Encoding", "gzip, deflate")
        .execute
        .body
    }
  }

  /** Get the title for a web page.
    *
    * @param theURL the URL for which we want the title
    * @param message the IRC message requesting the title
    * @return Unit - This method sends the title to the bot, itself.
    */
  def fetchTitle(theURL: String, message: IRCMessage) {
    val f = getFutureForURL(theURL)
    f onSuccess {
      case document => {
        val title = document.title.replace("\n", "").replaceAll("""\s+""", " ")
        if (!title.isEmpty)
          message.bot.sendMessage(message.channel, "\"" + title + "\"")
      }
    }
  }
}
