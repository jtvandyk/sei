# INSTRUCTIONS
# This script is for conducting segregation analysis on states with only Unified school districts. 
#
# Run through the following code step by step. In some cases the code creates
# an output that will need to be returned to the database. In other cases the output 
# is a form of analysis which will be used to construct the next table. 

#######################
# System Requirements #
#######################
# Load required packages
require(RPostgreSQL)
require(rgeos)
require(rgdal)
require(sp)
require(spdep)
require(igraph)

# Load PostgreSQL Driver
drv <- dbDriver("PostgreSQL")

# Open a PostgreSQL Connection
con <- dbConnect(drv, 
                 dbname="eb1",
                 host = "edbuild1.c85mgedxi7oy.us-east-1.rds.amazonaws.com",
                 port = 5432,
                 user = "ebadmin",
                 password = "Edbuild2014"
)

# Define state for schema
schema <- # " two character FIPS code " 

# Naming conventions for tables to load from database
unified.geo <- paste(schema, "_unified", sep="")
state.data  <- paste(schema, "_data", sep="")

# Naming conventions for tables to write back to database
db.edge.analysis        <- paste(schema, "_edgeanalysis", sep="")
db.edge.actual          <- paste(schema, "_edge_actual", sep="")
db.edge.adjusted        <- paste(schema, "_edge_adjusted", sep="")
db.node.strength        <- paste(schema, "_node_strength", sep="")                # Node Strength for all Districts
db.segregated.districts <- paste(schema, "_segregated_districts", sep="")   # Segregated Districts based on inputs
db.communities          <- paste(schema, "_communities", sep="")            # Community Detection Algorithm - Louvain

#################
# Create Tables #
#################

# Import relevant data tables for state
# This might be broken out into seperate scripts
unified <- dbReadTable(con, c("sei",unified.geo))       # Unified Geospatial Table // _unified
state_data <- dbReadTable(con, c("sei",state.data))       # Data Table // _data

# Reset row names
row.names(unified)    = unified$id.nces

#########################################################################
# Transition WKT to Spatial Polygon Dataframe for each type of district #
#########################################################################

## Change generic "state" to reflect unified, elementary, or secondary ##
## Code but comment out elementary and secondary to be unlocked as needed on state by state basis ##

# Create spatial polygons
# To set the PROJ4 string, enter the EPSG SRID and uncomment the 
# following two lines: SRID set to WGS84/4326
EPSG = make_EPSG()
p4s = EPSG[which(EPSG$code == 4326), "prj4"]

# For loop to iterate through each row in state table loaded above. 
# Creates sp class SpatialPolygonsDataFrame titled 'spTemp'
for (i in seq(nrow(unified))) {
  if (i == 1) {
    spTemp = readWKT(unified$wktTest[i], unified$id.nces[i])
    # If the PROJ4 string has been set, use the following instead
    # spTemp = readWKT(dfTemp$wkt_geometry[i], dfTemp$gid[i], p4s)
  }
  else {
    spTemp = rbind(
      spTemp, readWKT(unified$wktTest[i], unified$id.nces[i])
      # If the PROJ4 string has been set, use the following instead
      # spTemp, readWKT(dfTemp$wkt_geometry[i], dfTemp$gid[i], p4s)
    )
  }
}

# Create SpatialPolygonsDataFrame, drop WKT field from attributes
state_sp <- SpatialPolygonsDataFrame(spTemp, unified[-2])

# Verify that SpatialPolygonsDataFrame written properly
plot(state_sp)

##############################################
# Create spatial weights and neighbors lists #
##############################################

# Create neighbors list for SpatialPolygonsDataFrame (spdep)
state_nb <- poly2nb(state_sp,  row.names= state_sp$id.nces, queen = FALSE)

# Verify that NB written properly and get preliminary sample statistics
summary(state_nb)
state_coords <- coordinates(state_sp)
plot(state_nb, state_coords)

# Create weights for nb class neighbors list (spdep)
state_lw <- nb2listw(state_nb)

# Build adjacency table (spdep)
# Transition NB list/class to NB list weights
# Creates nb class weights list titled 'nbTemp'
nbTemp <- as(nb2listw(state_nb, style="B", zero.policy=TRUE), "CsparseMatrix")

# Build adjacency matrix table (igraph)
# Transition NB list weights to adjacency graph
state_adj <- graph.adjacency(nbTemp, mode="undirected")

# Test matrix graph
plot(state_adj)

# Transform to edgelist table (igraph)
# Create edge list with District NCES ID from adjacency graph
state_edge <- get.edgelist(state_adj, names=TRUE)
# names(state_edge) <- c("Source","Target") #Doesn't rename an edgelist

#######################
# Create Edge Weights #
#######################

# Create data frame from edgelist
state_dfedge <- data.frame(state_edge)
names(state_dfedge) <- c("source", "target")
state_dfedge$id <- 1:nrow(state_dfedge)

# Subset source and target nodes to join poverty rate data
state_source <- state_dfedge[c(1,3)]
state_target <- state_dfedge[c(2,3)]

# Subset from data table
names(state_data) <- c("gid", "state.fips", "id.nces", "name", "state.post", "est.pop", "est.student", "est.student.pov", "student.pov.rate")
subset_vars <- c("id.nces","student.pov.rate")
state_pov <- state_data[subsetVars]

# Merge poverty data with Source and Target nodes tables
state_source_data <- merge(state_source, state_pov, by.x = "source", by.y="id.nces")
state_target_data <- merge(state_target, state_pov, by.x = "target", by.y="id.nces")
state_edgeanalysis <- merge(state_source_data, state_target_data, by="id")
names(state_edgeanalysis) <- c("id","source","source.pov","target","target.pov")

# Absolute value in difference between Source and Target nodes
state_edgeanalysis$weight.actual <- abs(state_edgeanalysis$source.pov-state_edgeanalysis$target.pov)
state_edgeanalysis$weight.adjusted <- 1 - state_edgeanalysis$weight.actual        # inverting value of edge weight

###### The latter half of the schema below needs to feature pasting in the defined state code at the top of this script
###### Then the table and suffix will be concatenated to the two letter FIPS code. 
dbWriteTable(con, c("sei",db.edge.analysis), state_edgeanalysis, row.names=FALSE) 

# Subset edgeweight analysis table to new data frame for conversion
weight_vars_actual <- c("source","target","weight.actual")
state_edge_actual <- state_edgeanalysis[weight_vars_actual] # data frame with only source, target, and edge weight

weight_vars_adjusted <- c("source","target","weight.adjusted")
state_edge_adj <- state_edgeanalysis[weight_vars_adjusted] # data frame with only source, target, and edge weight

# Name state acronym here for writing to RDS
# Datatable with just source, target, and weights for future analysis
dbWriteTable(con, c("sei",db.edge.actual), state_edge_actual, row.names=FALSE)
dbWriteTable(con, c("sei",db.edge.adjusted), state_edge_adjusted, row.names=FALSE)

####################
# Network Analysis #
####################

############################
# Edge List Actual Weights #
# Convert to actual weighted edge list and then create undirected graph with data
el.actual <- as.matrix(state_edge_actual[,1:2])
state_g_actual <- graph.edgelist(el.actual, directed=FALSE)
E(state_g_actual)$weight = as.numeric(state_edge_actual[,3]) 

# Test undirected weighted network
plot(state_g_actual,layout=layout.fruchterman.reingold,edge.width=E(state_g_actual)$weight)

##############################
# Edge List Adjusted Weights #
# Convert to adjusted weighted edge list and then create undirected graph with data
el.adjusted <- as.matrix(state_edge_adjusted[,1:2])
state_g_adjusted <- graph.edgelist(el.adjusted, directed=FALSE)
E(state_g_adjusted)$weight = as.numeric(state_edge_adjusted[,3]) 

# Test undirected weighted network
plot(state_g_adjusted,layout=layout.fruchterman.reingold,edge.width=E(state_g_adjusted)$weight)

######################
# Node list creation #
######################
# This section isn't relevant for the thrust of work at the moment and it's incomplete nature reflects
# that outlook. Additions will be pushed over time as more layers of analysis are conducted within
# a network setting. For the time being this can be skipped.

# Subset qualitative data from state_data for creation of node list

############################
# Network based statistics #
############################

# Node Strength for Outbound Links (even though it's a undirected network) of Actual_Graph
state_node_strength <- graph.strength(state_g_actual, mode="out")
dbWriteTable(con, c("sei",db.node.strength), state_node_strength, row.names=FALSE)

# Identify edges that segregate with threshold factor
# For Dataframe - State Edge Actual
segregated_borders <- state_edge_actual[ which(state_edge_actual$weight > .20), ]         # This row is where the threshold is set - current value set to .2
dbWriteTable(con, c("sei",db.segregated.districts), segregated_borders, row.names=FALSE)

# Identify communities within weighted undirected network
segregated_communities <- cluster_louvain(state_g_actual)
dbWriteTable(con, c("sei",db.communities), segregated_communities, row.names=FALSE)
