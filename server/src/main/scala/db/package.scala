import io.getquill.{CamelCase, JdbcContext, SqliteDialect}

package object db {

  type DbContext = JdbcContext[SqliteDialect, CamelCase]

}