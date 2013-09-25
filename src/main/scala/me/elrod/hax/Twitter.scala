package me.elrod.hax

import twitter4j.conf.ConfigurationBuilder
import twitter4j.TwitterFactory

case class Twitter(key: String, secret: String) {
  private val builder = new ConfigurationBuilder
  builder setUseSSL true
  builder setApplicationOnlyAuthEnabled true

  val twitter = new TwitterFactory(builder.build()).getInstance
  twitter.setOAuthConsumer(key, secret)
  twitter.getOAuth2Token
}
