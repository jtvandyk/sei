#Required Packages
require(RPostgreSQL)
require(rgdal)
require(rgeos)
require(sp)

## loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")

## Open a connection
con <- dbConnect(drv, 
                 dbname="eb1",
                 host = "edbuild1.c85mgedxi7oy.us-east-1.rds.amazonaws.com",
                 port = 5432,
                 user = "ebadmin",
                 password = "Edbuild2014"
)

#Read table from database into R data frame
nj <- dbReadTable(con, c("sei","test2"))

# Close PostgreSQL connection 
dbDisconnect(con)