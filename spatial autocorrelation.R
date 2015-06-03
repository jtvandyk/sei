# INSTRUCTIONS
# Run through the following code step by step. In some cases the code creates
# output that will need to be returned to the database. In other cases the output 
# is a form of analysis which will be used to construct the next table. 

#####################
#System Requirements#
#####################
#Load required packages
require(RPostgreSQL)
require(rgeos)
require(rgdal)
require(sp)
require(spdep)
require(igraph)

# Load PostgreSQL Driver
drv <- dbDriver("PostgreSQL")

## Open a PostgreSQL Connection
con <- dbConnect(drv, 
                 dbname="eb1",
                 host = "edbuild1.c85mgedxi7oy.us-east-1.rds.amazonaws.com",
                 port = 5432,
                 user = "ebadmin",
                 password = "Edbuild2014"
)

###############
#Create Tables#
###############

# Import relevant data table for state
# Read table into R data frame
NJ <- dbReadTable(con, c("sei","test2"))

#Reset row names
row.names(NJ) = NJ$dNCESID

# Create spatial polygons
# To set the PROJ4 string, enter the EPSG SRID and uncomment the 
# following two lines: SRID set to WGS84/4326
EPSG = make_EPSG()
p4s = EPSG[which(EPSG$code == 4326), "prj4"]

#For loop to iterate through each row in state table loaded above. 
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

# Verify that SpatialPolygonsDataFrame written properly
plot(NJ.sp)

# Write new spatial class to database
# Create array to indicate schema, table where c("schema","table")
# dbWriteTable(con, c("sei","NJ.sp"), NJ.sp, row.names=FALSE) 

# dbWriteTable variants:
#   overwrite=TRUE : *replaces* table
#   append=TRUE    : inserts new rows
# (default is that both are FALSE; can't have both TRUE)

#Create neighbors list for SpatialPolygonsDataFrame (spdep)
NJ.nb <- poly2nb(NJ.sp,  row.names= NJ.sp$dNCESID, queen = FALSE)
# dbWriteTable(con, c("sei", "NJ.nb"), NJ.nb, row.names=FALSE)

#Create weights for nb class neighbors list (spdep)
NJ.lw <- nb2listw(NJ.nb)
# dbWriteTable(con, c("sei","NJ.lw"), NJ.lw, row.names=FALSE)

#Build adjacency table (spdep)
#Transition NB list/class to NB list weights
B <- as(nb2listw(NJ.nb, style="B", zero.policy=TRUE), "CsparseMatrix")

#Build adjacency matrix table (igraph)
#Transition NB list weights to adjacency graph
NJ.testmatrix <- graph.adjacency(B, mode="undirected")

#Test matrix graph
plot(NJ.testmatrix)

#Transform to edgelist table (igraph)
#Create edge list with District NCES ID from adjacency graph
NJ.testedge <- get.edgelist(NJ.testmatrix, names=TRUE)

# Close PostgreSQL connection 
dbDisconnect(con)