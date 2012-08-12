package me.elrod.hax
import scala.concurrent.{ Future, future }
import scala.concurrent.ExecutionContext.Implicits.global
import org.jsoup.Jsoup

object URLSnarfers {
  val URLRegex = """(?i).*?(https?://\S+).*""".r

  def fetchTitle(theURL: String, bot: Hax, channel: String) {
    val f: Future[Option[String]] = future {
      val document = Jsoup.connect(theURL)
        .header("Accept-Language", "en-us,en;q=0.5")
        .header("Accept-Encoding", "gzip, deflate")
        .get()
      val title = document.title.replace("\n", "").replaceAll("""\s+""", " ")
      if (!title.isEmpty) Some("\"" + title + "\"") else None
    }
    f onSuccess {
      case e => e match {
        case Some(e) => bot.sendMessage(channel, e)
        case _ =>
      }
    }
  }
}
