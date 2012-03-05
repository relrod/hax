package me.elrod.hax.urlSnarfing
import scalaj.http.{Http,HttpOptions}
import org.jsoup.Jsoup
import org.scala_tools.time.Imports._
import net.liftweb.json._

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

  /** Return a string containing information from the Spotify API about a given spotify URI.
    *
    * @param mediaType the type of media to ask the API about
    * @param identifier the unique ID of the Spotify item
    */
  def spotifyInfo(mediaType: String, identifier: String): String = {
    try {
      val http = Http("http://ws.spotify.com/lookup/1/?uri=spotify:" + mediaType + ":" + identifier).option(HttpOptions.connTimeout(2000)).option(HttpOptions.readTimeout(2000))
      val document = Jsoup.parse(http.asString)

      mediaType match {
        case "track" => {
          val name = document.select("name").first.text
          val artist = document.select("artist name").text
          val album = document.select("album name").text
          val trackNumber = document.select("track-number").text.toInt
          val length = document.select("length").text.toDouble
          val popularity = document.select("popularity").text.toDouble
          
          // Convert time to something parsable.
          val lengthJoda = Period.millis((length * 1000).toInt).normalizedStandard
          "%s - %s (Album: %s [track %d]) (Popularity: %1.5f) (Length: %02d:%02d)".format(artist, name, album, trackNumber, popularity, lengthJoda.getMinutes, lengthJoda.getSeconds)
        }
        case "artist" => document.select("name").first.text
        case "album" => {
          val name = document.select("name").first.text
          val artistName = document.select("artist name").text
          val released = document.select("released").text
          "Album: %s - Artist: %s - Released: %s".format(name, artistName, released)
        }
      }
    } catch {
      case e: java.net.SocketTimeoutException => "<timeout>"
      case unknown => "<error> " + unknown
    }
  }

  /** Return information about a youtube video.
    *
    * @param the YouTube video ID
    */
  def youtubeInfo(videoID: String): String = {
    try {
      val http = Http("https://gdata.youtube.com/feeds/api/videos?alt=jsonc&v=2&max-results=1&q=" + videoID).option(HttpOptions.connTimeout(4000)).option(HttpOptions.readTimeout(4000)).asString
      val json: JValue = parse(http)
      val lengthJoda = Period.seconds(compact(render(json \ "data" \ "items" \ "duration")).toInt).normalizedStandard
      "\"%s\" [%02d:%02d] by %s (%,d views%s)".format(
        (json \ "data" \ "items" \ "title").values.toString,
        lengthJoda.getMinutes,
        lengthJoda.getSeconds,
        (json \ "data" \ "items" \ "uploader").values.toString,
        compact(render(json \ "data" \ "items" \ "viewCount")).toInt,
        (json \ "data" \ "items" \ "rating") match {
          case JNothing => ""
          case value => ", %1.3f%% thumbs-up".format((compact(render(value)).toDouble / 5) * 100)
        }
      )
    } catch {
      case e: java.net.SocketTimeoutException => "<timeout>"
      case unknown => "<error> " + unknown
    }
  }

}
