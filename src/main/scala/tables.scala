package me.elrod.hax.tables
import org.scalaquery.ql.TypeMapper._
import org.scalaquery.ql.basic.BasicTable

// Tables.
object Karma extends BasicTable[(Int, String, Int)]("karma") {
  def id = column[Int]("id", O PrimaryKey, O NotNull)
  def item = column[String]("item")
  def karma = column[Int]("karma", O Default 0)
  def * = id ~ item ~ karma
}

object Quote extends BasicTable[(Int, String, String, String, String)]("quotes") {
  def id = column[Int]("id", O PrimaryKey, O NotNull)
  def quote = column[String]("quote", O NotNull)
  def added_by = column[String]("added_by")
  def channel = column[String]("channel")
  def timestamp = column[String]("timestamp", O NotNull, O DBType "text")
  def * = id ~ quote ~ added_by ~ channel ~ timestamp
}
