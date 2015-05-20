#Required Packages
require(RPostgreSQL)
require(rgeos)

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
nj <- dbReadTable(con, c("sei","NJ"))

#Verify that state$geom is present
str(nj)

#Translate data frame into SP class object(SpatialPolygons, SPatialPoints, or SpatialLines)
#with readWKT function in rgeos



# Close PostgreSQL connection 
dbDisconnect(con)