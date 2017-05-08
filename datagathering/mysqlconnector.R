#localmysqlserverconnection.

#install.packages("RMySQL")
#?RMySQL
library(RMySQL)

#You have to create a mysql server instance with a db named unicorn for this line to work.
mydb = dbConnect(MySQL(), user='unicorn', password='n7gtRLHi', dbname='unicorn', host='ec2-54-85-232-216.compute-1.amazonaws.com')
#Once you have that, this will create all the tables you need and import the initial stock list.

dbWriteTable(conn, name, value,
             field.types = NULL, row.names = TRUE, overwrite = FALSE,
             append = FALSE, ..., allow.keywords = FALSE)

#https://www.r-bloggers.com/mysql-and-r/
#https://cran.r-project.org/web/packages/RMySQL/RMySQL.pdf

  
#Reading and writing tables to the DB
#dbWriteTable(conn, name, value,
#             field.types = NULL, row.names = TRUE, overwrite = FALSE,
#             append = FALSE, ..., allow.keywords = FALSE)

#dbReadTable()

#Executing a query
#dbGetQuery()
#dbReadTable()  - Pulls back whole table
#dbWriteTable()



###############

#Read from DB
#loadportfolio
#loadfeatures
dbReadTable(mydb,"unicorn_portfolios")
#
#Write from DB

