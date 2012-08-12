package me.elrod.hax
import scala.concurrent.{ Future, future }
import scala.concurrent.ExecutionContext.Implicits.global
import org.jsoup.Jsoup

object URLSnarfers {
  val URLRegex = """(?i).*?(https?://\S+).*""".r

  def getFutureForURL(theURL: String) = {
    future {
      Jsoup.connect(theURL)
        .header("Accept-Language", "en-us,en;q=0.5")
        .header("Accept-Encoding", "gzip, deflate")
        .get()
    }
  }

  def fetchTitle(theURL: String, bot: Hax, channel: String) {
    val f = getFutureForURL(theURL)
    f onSuccess {
      case document => {
        val title = document.title.replace("\n", "").replaceAll("""\s+""", " ")
        if (!title.isEmpty) bot.sendMessage(channel, "\"" + title + "\"")
      }
    }
  }
}
