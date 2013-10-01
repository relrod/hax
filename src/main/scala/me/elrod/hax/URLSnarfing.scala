package me.elrod.hax
import scala.concurrent.{ Future, future }
import scala.concurrent.ExecutionContext.Implicits.global
import org.jsoup.Jsoup
import net.liftweb.json._
import org.joda.time.Period
import scala.collection.JavaConversions._
import org.apache.commons.lang3.StringEscapeUtils.unescapeHtml4

object URLSnarfers {
  val URLRegex = """(?i).*?(https?://\S+).*""".r
  val SpotifyRegex = """(?i).*spotify:(\w+):(\w+).*""".r
  val TwitterRegex = """(?i).*?https?://twitter.com/.*/status(?:es|)/(\d+).*""".r

  /** Concurrently get a JSoup Document with the data from the URL.
    *
    * This uses JSoup to go out and get the content of a URL, then calls get()
    * on it and returns the result as a Scala (2.10.0+) Future
    *
    * @param theURL the URL to be fetched
    * @return a future that, on success, will contain a
    *         [[org.jsoup.nodes.Document]] with the response.
    */
  def getFutureForURL(theURL: String,
    params: Option[Map[String, String]] = None) = {

    val jsoup = Jsoup.connect(new java.net.URI(theURL).toASCIIString())
      .header("Accept-Language", "en-us,en;q=0.5")
      .header("Accept-Encoding", "gzip, deflate")

    params match {
      case Some(map) => for ((name, value) <- map) {
        jsoup.data(name, value)
      }
      case _ =>
    }

    future {
      jsoup.get()
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
  def getFutureRawBodyForURL(theURL: String,
    params: Option[Map[String, String]] = None) = {

    val jsoup = Jsoup.connect(new java.net.URI(theURL).toASCIIString())
      .ignoreContentType(true)
      .header("Accept-Language", "en-us,en;q=0.5")
      .header("Accept-Encoding", "gzip, deflate")

    params match {
      case Some(map) => for ((name, value) <- map) {
        jsoup.data(name, value)
      }
      case _ =>
    }

    future {
      jsoup.execute.body
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

  /** Fetch information about a given tweet, based on its id.
   *
   * @param twitter the Twitter instance to use
   * @param id the tweet ID
   * @param message The irc message that the link came from
   * @return Unit - This method sends the result to the bot, itself.
   */
  def fetchTweet(twitter: twitter4j.Twitter, id: String, message: IRCMessage) {
    future {
      twitter showStatus id.toLong
    } onSuccess {
      case tweet => {
        val formatted = "\002@%s\002's tweet (fav: %d, rt: %d): %s".format(
          tweet.getUser.getScreenName,
          tweet.getFavoriteCount,
          tweet.getRetweetCount,
          unescapeHtml4(tweet.getText.replaceAll("\n", " ")))
        message.bot.sendMessage(message.channel, formatted)
      }
    }
  }

  /** Get information about Spotify URIs.
    *
    * Spotify, an online music service (http://spotify.com/), provides an API
    * which allows you to query their database for spotify URLs and get
    * information about a song, album, artist, etc.
    *
    * Here, we take advantage of this API and allow song info to be displayed
    * in-channel.
    *
    * @param mediaType the type of media we want to ask the API about
    * @param identifier the unique identifier of the song/artist/album
    * @param message the IRC message requesting the title
    * @return Unit - This method sends the result to the bot, itself.
    */
  def spotifyURI(mediaType: String, identifier: String, message: IRCMessage) {
    val f = getFutureForURL(
      "http://ws.spotify.com/lookup/1/?uri=spotify:%s:%s".format(
        mediaType,
        identifier))

    f onSuccess {
      case document => {
        val response = mediaType match {
          case "track" => {
            val name = document.select("name").first.text
            val artist = document.select("artist").iterator.map(_.select("name").text).mkString(", ")
            val album = document.select("album name").text
            val trackNumber = document.select("track-number").text.toInt
            val length = document.select("length").text.toDouble
            val popularity = document.select("popularity").text.toDouble

            // Convert time to something parsable.
            val lengthJoda = Period.millis((length * 1000).toInt)
              .normalizedStandard

            "%s - %s (Album: %s [track %d]) (Popularity: %1.5f) (Length: %02d:%02d)".format(
                artist,
                name,
                album,
                trackNumber,
                popularity,
                lengthJoda.getMinutes,
                lengthJoda.getSeconds)
          }
          case "artist" => document.select("name").first.text
          case "album" => {
            val name = document.select("name").first.text
            val artistName = document.select("artist name").text
            val released = document.select("released").text
            "Album: %s - Artist: %s - Released: %s".format(
              name,
              artistName,
              released)
          }
        }
        message.bot.sendMessage(message.channel, response)
      }
    }
  }
}
