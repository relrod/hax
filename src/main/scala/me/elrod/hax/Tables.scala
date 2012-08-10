package me.elrod.hax
import scala.slick.driver.SQLiteDriver.simple._

object Karma extends Table[(Int, String, Int)]("karma") {
  def id = column[Int]("id", O.PrimaryKey)
  def item = column[String]("item")
  def count = column[Int]("count", O.Default(0))
  def * = id ~ item ~ count
}

