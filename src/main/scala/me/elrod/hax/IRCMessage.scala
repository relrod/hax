package me.elrod.hax

case class IRCMessage(bot: Hax,
  channel: String,
  sender: String,
  login: String,
  hostname: String,
  message: String)
