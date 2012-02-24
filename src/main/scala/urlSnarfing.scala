package me.elrod.hax.urlSnarfing
import dispatch.{thread, Http, HttpsLeniency, url}
import dispatch.jsoup.JSoupHttp._

object urlSnarfing {
  /** Return the title of a given URL.
    *
    * Fetch the contents of a <title>...</title> tag from a URL using
    * the Dispatch/JSoup set of libraries.
    *
    * @param theURL the URL to snarf the title from
    */
  def fetchURLTitle(theURL: String): String = {
    val https = new Http with HttpsLeniency
    val title = https(url(theURL) </> { html => html.title }).toString
    if (!title.isEmpty)
      "\"" + title.replace("\n", "").replaceAll("""\s+""", " ") + "\""
    else
      ""
  }

  /** Return a tweet from a given twitter status URL.
    *
    * @param tweetID the ID of the tweet to obtain
    */
  def fetchTweet(tweetID: String): String = {
    val https = new Http with HttpsLeniency
    val twitterURL = "http://api.twitter.com/1/statuses/show.xml?id=" + tweetID
    https(url(twitterURL) </> { status => status.select("text").text }).toString
  }

}
