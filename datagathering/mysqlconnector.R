#localmysqlserverconnection.

#install.packages("RMySQL")
#?RMySQL
library(RMySQL)

#You have to create a mysql server instance with a db named unicorn for this line to work.
mydb = dbConnect(MySQL(), user='root', password='password', dbname='unicorn', host='localhost')

#Once you have that, this will create all the tables you need and import the initial stock list.
dbListTables(mydb)

dbWriteTable(conn, name, value,
             field.types = NULL, row.names = TRUE, overwrite = FALSE,
             append = FALSE, ..., allow.keywords = FALSE)

#https://www.r-bloggers.com/mysql-and-r/
  
#Reading and writing tables to the DB
#dbWriteTable(conn, name, value,
#             field.types = NULL, row.names = TRUE, overwrite = FALSE,
#             append = FALSE, ..., allow.keywords = FALSE)

#dbReadTable()