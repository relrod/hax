package me.elrod.hax

/** Handle Hax command parsing. */
object Command {
  
  /** Commands without arguments.
   *
   * @param command the command that was called
   * @return an Option[String] containing the response
   */
  def apply(command: String): Option[String] = command match {
    case "time" => Some(new java.util.Date().toString)
    case "meme" => Some(io.Source.fromURL("http://api.automeme.net/text")
                            .mkString
                            .lines
                            .toList
                            .head)
    case c => None
  }

  /** Commands with arguments.
    *
    * @param command the command that was called
    * @param arguments a String containing arguments for the command
    * @return an Option[String] containing the response
    */
  def apply(command: String, arguments: String): Option[String] = {
    command.drop(0) match {
      case "slap" | "fishify" =>
        Some("\001ACTION slaps %s with a ><>.\001".format(arguments))
      case _ => None
    }
  }
}
