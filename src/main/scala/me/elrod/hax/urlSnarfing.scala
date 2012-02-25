package me.elrod.hax.urlSnarfing
import scalaj.http.{Http,HttpOptions}
import org.jsoup.Jsoup

object urlSnarfing {
  /** Return the title of a given URL.
    *
    * Fetch the contents of a <title>...</title> tag from a URL using
    * the Dispatch/JSoup set of libraries.
    *
    * @param theURL the URL to snarf the title from
    */
  def fetchURLTitle(theURL: String): String = {
    try {
      val http = Http(theURL).option(HttpOptions.allowUnsafeSSL).option(HttpOptions.connTimeout(2000)).option(HttpOptions.readTimeout(2000))
      val document = Jsoup.parse(http.asString)
      "\"" + document.title.replace("\n", "").replaceAll("""\s+""", " ") + "\""
    } catch {
      case e: java.net.SocketTimeoutException => "<timeout>"
      case unknown => "<error> " + unknown
    }
  }

  /** Return a tweet from a given twitter status URL.
    *
    * @param tweetID the ID of the tweet to obtain
    */
  def fetchTweet(tweetID: String): String = {
    try {
      val http = Http("https://api.twitter.com/1/statuses/show.xml?id=" + tweetID).option(HttpOptions.connTimeout(2000)).option(HttpOptions.readTimeout(2000))
      val document = Jsoup.parse(http.asString)
      document.select("text").text
    } catch {
      case e: java.net.SocketTimeoutException => "<timeout>"
      case unknown => "<error> " + unknown
    }
  }

}
