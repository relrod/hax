import org.scalatest.FunSpec
import me.elrod.hax.urlSnarfing.urlSnarfing._

class UrlSnarfingSpecs extends FunSpec {
  describe("YouTube URL Snarfing from a given Video ID") {
    it("should correctly retrieve the title of a video") {
      assert(youtubeInfo("05X6vKg5eCM").startsWith("\"Zack's Voiture Ferme\""))
    }

    it("should be able to retrieve info from videos that have dashes in them") {
      assert(youtubeInfo("WSeNSzJ2-Jw").startsWith("\"SKRILLEX - Scary"))
    }
  }

  describe("URL title tag snarfing") {
    it("should be able to get the title of Google's homepage over http") {
      assert(fetchURLTitle("http://www.google.com") == "\"Google\"")
    }

    it("should be able to get the title of Google's homepage over https (ssl)") {
      assert(fetchURLTitle("https://www.google.com") == "\"Google\"")
    }

    it("should gracefully handle a hostname that doesn't exist") {
      assert(fetchURLTitle("http://does.not.exist.123.abc").startsWith("<error> java.net.UnknownHostException"))
    }

    it("should gracefully handle connecting via ssl to a non ssl site") {
      assert(fetchURLTitle("https://elrod.me").startsWith("<error> javax.net.ssl"))
    }

    it("should gracefully handle not connecting due to a bad given port") {
      assert(fetchURLTitle("http://elrod.me:1234").startsWith("<error> java.net.NoRouteToHostException"))
    }

    it("should remain silent if there is no title tag") {
      assert(fetchURLTitle("http://elrod.me/CodeBlock.jpg") == "")
    }
  }

  describe("Spotify URL Snarfing") {
    it("should correctly identify a song based on track") {
      assert(spotifyInfo("track", "0ac0R0wkioYDhzQDbCFokO").startsWith("Rick Astley - Never Gonna Give You Up"))
    }

    it("should correctly identify an artist based on their ID") {
      assert(spotifyInfo("artist", "292sg99iIOc93zcd30r4Oz") == "Jonathan Coulton")
    }

    it("should correctly identify an album based on its ID") {
      assert(spotifyInfo("album", "2zAppH7uElAArIWkCUJrtW").startsWith("Album: 60's Rock Instrumental"))
    }

    it("should fail gracefully on an invalid ID for any media type") {
      assert(spotifyInfo("track", "lololololol").startsWith("<error>"))
      assert(spotifyInfo("album", "lololololol").startsWith("<error>"))
      assert(spotifyInfo("artist", "lololololol").startsWith("<error>"))
    }
  }

  describe("Twitter URL Snarfing") {
    it("should correctly show a tweet for a given tweet ID") {
      assert(fetchTweet("168353858286923779").startsWith("WebKit is about to switch"))
    }

    it("should fail gracefully on an invalid tweet ID") {
      assert(fetchTweet("lol123").startsWith("<error>"))
    }
  }

}
