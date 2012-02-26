package me.elrod.hax.urlSnarfing
import scalaj.http.{Http,HttpOptions}
import org.jsoup.Jsoup
import org.scala_tools.time.Imports._

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
      val http = Http(theURL).option(HttpOptions.allowUnsafeSSL).option(HttpOptions.connTimeout(4000)).option(HttpOptions.readTimeout(4000))
      val document = Jsoup.parse(http.asString)
      val title = document.title.replace("\n", "").replaceAll("""\s+""", " ")
      if (!title.isEmpty) "\"" + title + "\""
      else ""
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

  /** Return a string containing track info from a Spotify track ID
    *
    * @param trackNumber the unique ID of the Spotify track
    */
  def spotifyTrackInfo(trackID: String): String = {
    try {
      val http = Http("http://ws.spotify.com/lookup/1/?uri=spotify:track:" + trackID).option(HttpOptions.connTimeout(2000)).option(HttpOptions.readTimeout(2000))
      val document = Jsoup.parse(http.asString)
      val name = document.select("name").first.text
      val artist = document.select("artist name").text
      val album = document.select("album name").text
      val trackNumber = document.select("track-number").text.toInt
      val length = document.select("length").text.toDouble
      val popularity = document.select("popularity").text.toDouble
      
      // Convert time to something parsable.
      val lengthJoda = Period.millis((length * 1000).toInt).normalizedStandard
      "%s - %s (Album: %s [track %d]) (Popularity: %1.5f) (Length: %d minutes, %d seconds)".format(artist, name, album, trackNumber, popularity, lengthJoda.getMinutes, lengthJoda.getSeconds)
    } catch {
      case e: java.net.SocketTimeoutException => "<timeout>"
      case unknown => "<error> " + unknown
    }
  }

}
