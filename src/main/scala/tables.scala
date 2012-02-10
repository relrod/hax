import org.scalaquery.ql.TypeMapper._
import org.scalaquery.ql._
import org.scalaquery.ql.basic.BasicTable

// Tables.
object Karma extends BasicTable[(Int, String, Int)]("karma") {
  def id = column[Int]("id", O PrimaryKey, O NotNull)
  def item = column[String]("item", O DBType "varchar(255)")
  def karma = column[Int]("karma", O Default 0)
  def * = id ~ item ~ karma
}
