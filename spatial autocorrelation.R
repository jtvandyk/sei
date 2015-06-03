# INSTRUCTIONS
# Run through the following code step by step. In some cases the code creates
# an output that will need to be returned to the database. In other cases the output 
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

# Import relevant data tables for state
NJ <- dbReadTable(con, c("sei","test2"))    # Geospatial Table
NJ.data <- dbReadTable(con, c("sei","NJ"))  # Data Table

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

# Create neighbors list for SpatialPolygonsDataFrame (spdep)
NJ.nb <- poly2nb(NJ.sp,  row.names= NJ.sp$dNCESID, queen = FALSE)

summary(NJ.nb)
plot(NJ.nb)

# Create weights for nb class neighbors list (spdep)
NJ.lw <- nb2listw(NJ.nb)

# Build adjacency table (spdep)
# Transition NB list/class to NB list weights
# Creates nb class weights list titled 'nbTemp'
nbTemp <- as(nb2listw(NJ.nb, style="B", zero.policy=TRUE), "CsparseMatrix")

# Build adjacency matrix table (igraph)
# Transition NB list weights to adjacency graph
NJ.adj <- graph.adjacency(nbTemp, mode="undirected")

# Test matrix graph
plot(NJ.adj)

# Transform to edgelist table (igraph)
# Create edge list with District NCES ID from adjacency graph
NJ.edge <- get.edgelist(NJ.adj, names=TRUE)
names(NJ.edge) <- c("Source","Target")

#####################
#Create Edge Weights#
#####################

# Create data frame from edgelist
NJ.dfedge <- data.frame(NJ.edge)
names(NJ.dfedge) <- c("Source", "Target")
NJ.dfedge$ID <- 1:nrow(NJ.dfedge)

# Subset source and target nodes to join poverty rate data
NJ.source <- NJ.dfedge[c(1,3)]
NJ.target <- NJ.dfedge[c(2,3)]

# Convert to data frame to join poverty rate
# NJ.source <- data.frame(NJ.source)
# NJ.target <- data.frame(NJ.target)

# Rename columns
# names(NJ.source) <- c("Source")
# names(NJ.target) <- c("Target")

# Subset from data table
subsetVars <- c("dNCESID","dEstPovRate")
NJ.pov <- NJ.data[subsetVars]

# Merge poverty data with Source and Target nodes tables
NJ.sourceData <- merge(NJ.source, NJ.pov, by.x = "Source", by.y="dNCESID")
NJ.targetData <- merge(NJ.target, NJ.pov, by.x = "Target", by.y="dNCESID")
NJ.edgeanalysis <- merge(NJ.sourceData, NJ.targetData, by="ID")
names(NJ.edgeanalysis) <- c("ID","Source","sourcePov","Target","targetPov")

# Absolute value in difference between Source and Target nodes
NJ.edgeanalysis$Weight <- abs(NJ.edgeanalysis$sourcePov-NJ.edgeanalysis$targetPov)

# Subset edgeweight analysis table to new data frame for conversion
weightVars <- c("Source","Target","Weight")
NJ.edgew <- NJ.edgeanalysis[weightVars]
NJ.edgew$Weight <- 1 - NJ.edgew$Weight
dbWriteTable(con, c("sei","NJedgew"), NJ.edgew, row.names=FALSE)

# Convert to weighted edge list
NJ.edgeW <- graph.data.frame(NJ.edgew)

# Convert to weighted adjacency matrix
NJ.adjW <- get.adjacency(NJ.edgeW, attr='Weight')

###########################
#Close Database Connection#
###########################

# Close PostgreSQL connection 
dbDisconnect(con)