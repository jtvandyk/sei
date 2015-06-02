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

#Reset row names
row.names(NJ) = NJ$dNCESID

# Create spatial polygons
# To set the PROJ4 string, enter the EPSG SRID and uncomment the 
# following two lines: SRID set to WGS84
EPSG = make_EPSG()
p4s = EPSG[which(EPSG$code == 4326), "prj4"]

#For loop to iterate through each row in table loaded above. 
#Creates sp class SpatialPolygonsDataFrame titled 'spTemp'
for (i in seq(nrow(NJ))) {
  if (i == 1) {
    spTemp = readWKT(NJ$wktTest[i], NJ$dNCESID[i])
    # If the PROJ4 string has been set, use the following instead
    #spTemp = readWKT(dfTemp$wkt_geometry[i], dfTemp$gid[i], p4s)
  }
  else {
    spTemp = rbind(
      spTemp, readWKT(NJ$wktTest[i], NJ$dNCESID[i])
      # If the PROJ4 string has been set, use the following instead
      # spTemp, readWKT(dfTemp$wkt_geometry[i], dfTemp$gid[i], p4s)
    )
  }
}

# Create SpatialPolygonsDataFrame, drop WKT field from attributes
NJ.sp <- SpatialPolygonsDataFrame(spTemp, NJ[-2])

#Verify that SpatialPolygonsDataFrame written properly
plot(NJ.sp)

#Write new spatial class to database
#Create array to indicate schema, table
dbWriteTable(con, c("sei","NJ.sp"), NJ.sp, row.names=FALSE) 

# dbWriteTable variants:
#   overwrite=TRUE : *replaces* table
#   append=TRUE    : inserts new rows
# (default is that both are FALSE; can't have both TRUE)

#Plot neighbors (spdep)
NJ.nb <- poly2nb(NJ.sp,  row.names= NJ.sp$dNCESID, queen = FALSE)
dbWriteTable(con, c("sei", "NJ.nb"), NJ.nb, row.names=FALSE)

#Create weights for nb class neighbors list (spdep)
NJ.lw <- nb2listw(NJ.nb)
dbWriteTable(con, c("sei","NJ.lw"), NJ.lw, row.names=FALSE)

#Build adjacency table (spdep)
NJ.adj <- nb2mat(NJ.nb, style = 'B')
dbWriteTable(con, c("sei", "NJ.adj"), NJ.adj, row.names=FALSE)

#Build adjacency matrix table (igraph)
NJ.e <- graph.adjacency(NJ.adj, mode="undirected")

#Transform to edgelist table (igraph)
NJ.edge <- get.edgelist(NJ.e, names=TRUE)
dbWriteTable(con, c("sei", "NJ.edge"), NJ.edge, row.names=FALSE)

# Close PostgreSQL connection 
dbDisconnect(con)