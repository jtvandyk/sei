# INSTRUCTIONS
# This script is for conducting segregation analysis on states with Unified, Elementary, and 
# Secondary school districts. 
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

unified.geo     <- paste(schema, "_unified", sep="")
secondary.geo   <- paste(schema, "_secondary", sep="")
elementary.geo  <- paste(schema, "_elementary", sep="")
state.data      <- paste(schema, "_data", sep="")

# Naming conventions for tables to write back to database
# Unified and Elementary Edgelists
ue.edge.analysis  <- paste(schema, "_ue_edgeanalysis", sep="")
ue.edge.actual    <- paste(schema, "_ue_edge_actual", sep="")
ue.edge.adjusted  <- paste(schema, "_ue_edge_adjusted", sep="")

# Unified and Secondary Edgelists
us.edge.analysis  <- paste(schema, "_us_edgeanalysis", sep="")
us.edge.actual    <- paste(schema, "_us_edge_actual", sep="")
us.edge.adjusted  <- paste(schema, "_us_edge_adjusted", sep="")

# Actual Absolute Value Statewide Edgelists
actual.edge.actual    <- paste(schema, "_edge_actual", sep="")

# Adjusted Absolute Value Statewide Edgelists
adjusted.edge.adjusted  <- paste(schema, "_edge_adjusted", sep="")

# Final Tables
db.node.strength        <- paste(schema, "_node_strength", sep="")          # Node Strength for all Districts
db.segregated.districts <- paste(schema, "_segregated_districts", sep="")   # Segregated Districts based on inputs
db.communities          <- paste(schema, "_communities", sep="")            # Community Detection Algorithm - Louvain

#################
# Create Tables #
#################

# Import relevant data tables for state
# This might be broken out into seperate scripts
unified     <- dbReadTable(con, c("sei", unified.geo))       # Unified Geospatial Table // _unified
secondary   <- dbReadTable(con, c("sei", secondary.geo))     # Secondary Geospatial Table // _secondary
elementary  <- dbReadTable(con, c("sei", elementary.geo))    # Elementary Geospatial Table //_elementary
state_data  <- dbReadTable(con, c("sei", state.data))        # Data Table //_data

# Reset row names
row.names(unified)    = unified$id.nces
row.names(secondary)  = secondary$id.nces
row.names(elementary) = elementary$id.nces

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

####################
# Loop for UNIFIED #
# For loop to iterate through each row in state table loaded above. 
# Creates sp class SpatialPolygonsDataFrame titled 'spTemp'
for (i in seq(nrow(unified))) {
  if (i == 1) {
    spTempU = readWKT(unified$wktTest[i], unified$id.nces[i])
    # If the PROJ4 string has been set, use the following instead
    # spTemp = readWKT(dfTemp$wkt_geometry[i], dfTemp$gid[i], p4s)
  }
  else {
    spTempU = rbind(
      spTempU, readWKT(unified$wktTest[i], unified$id.nces[i])
      # If the PROJ4 string has been set, use the following instead
      # spTemp, readWKT(dfTemp$wkt_geometry[i], dfTemp$gid[i], p4s)
    )
  }
}

unified_sp <- SpatialPolygonsDataFrame(spTempU, unified[-2])

# Verify that SpatialPolygonsDataFrame written properly
plot(unified_sp)

######################
# Loop for SECONDARY #
# For loop to iterate through each row in state table loaded above. 
# Creates sp class SpatialPolygonsDataFrame titled 'spTemp'
for (i in seq(nrow(secondary))) {
  if (i == 1) {
    spTempS = readWKT(secondary$wktTest[i], secondary$id.nces[i])
    # If the PROJ4 string has been set, use the following instead
    # spTemp = readWKT(dfTemp$wkt_geometry[i], dfTemp$gid[i], p4s)
  }
  else {
    spTempS = rbind(
      spTempS, readWKT(secondary$wktTest[i], secondary$id.nces[i])
      # If the PROJ4 string has been set, use the following instead
      # spTemp, readWKT(dfTemp$wkt_geometry[i], dfTemp$gid[i], p4s)
    )
  }
}

# Create SpatialPolygonsDataFrame, drop WKT field from attributes
secondary_sp <- SpatialPolygonsDataFrame(spTempS, secondary[-2])

# Verify that SpatialPolygonsDataFrame written properly
plot(secondary_sp)

#######################
# Loop for ELEMENTARY #
# For loop to iterate through each row in state table loaded above. 
# Creates sp class SpatialPolygonsDataFrame titled 'spTemp'
for (i in seq(nrow(elementary))) {
  if (i == 1) {
    spTempE = readWKT(elementary$wktTest[i], elementary$id.nces[i])
    # If the PROJ4 string has been set, use the following instead
    # spTemp = readWKT(dfTemp$wkt_geometry[i], dfTemp$gid[i], p4s)
  }
  else {
    spTempE = rbind(
      spTempE, readWKT(elementary$wktTest[i], elementary$id.nces[i])
      # If the PROJ4 string has been set, use the following instead
      # spTemp, readWKT(dfTemp$wkt_geometry[i], dfTemp$gid[i], p4s)
    )
  }
}

# Create SpatialPolygonsDataFrame, drop WKT field from attributes
elementary_sp <- SpatialPolygonsDataFrame(spTempE, elementary[-2])

# Verify that SpatialPolygonsDataFrame written properly
plot(elementary_sp)

##################################################
# Union Unified/Secondary and Unified/Elementary #
##################################################
# Transition these joined spatial polygon data frames into neighbor lists and list weights in the proceeding section
# Anticipate creating two seperate sets of weighted edge lists

# This will include weighted actual/adjusted edge list for Unified/Secondary and
# a weighted actual/adjusted edge list for Unified/Elementary.

# At the end of the analysis these edge lists will be stacked using gtools to create
# a comprehensive list of all relationships for a state. 

ue_sp <- gUnion(union, elementary, byid=TRUE, id=geoid)   # Union of Unified and Elementary School Districts
us_sp <- gUnion(union, secondary, byid=TRUE, id=geoid)    # Union of Unified and Secondary School Districts

##############################################
# Create spatial weights and neighbors lists #
##############################################

###################################
# Union + Elementary Data Created #
# Create neighbors list for SpatialPolygonsDataFrame (spdep)
ue_nb <- poly2nb(ue_sp,  row.names= ue_sp$id.nces, queen = FALSE)

# Verify that NB written properly and get preliminary sample statistics
summary(ue_nb)
ue_coords <- coordinates(ue_sp)
plot(ue_nb, ue_coords)

# Create weights for nb class neighbors list (spdep)
ue_lw <- nb2listw(ue_nb)

# Build adjacency table (spdep)
# Transition NB list/class to NB list weights
# Creates nb class weights list titled 'nbTemp'
ueTemp <- as(nb2listw(ue_nb, style="B", zero.policy=TRUE), "CsparseMatrix")

# Build adjacency matrix table (igraph)
# Transition NB list weights to adjacency graph
ue_adj <- graph.adjacency(ueTemp, mode="undirected")

# Test matrix graph
plot(ue_adj)

# Transform to edgelist table (igraph)
# Create edge list with District NCES ID from adjacency graph
ue_edge <- get.edgelist(ue_adj, names=TRUE)

##################################
# Union + Secondary Data Created #
# Create neighbors list for SpatialPolygonsDataFrame (spdep)
us_nb <- poly2nb(us_sp,  row.names= us_sp$id.nces, queen = FALSE)

# Verify that NB written properly and get preliminary sample statistics
summary(us_nb)
us_coords <- coordinates(us_sp)
plot(us_nb, us_coords)

# Create weights for nb class neighbors list (spdep)
us_lw <- nb2listw(us_nb)

# Build adjacency table (spdep)
# Transition NB list/class to NB list weights
# Creates nb class weights list titled 'nbTemp'
usTemp <- as(nb2listw(us_nb, style="B", zero.policy=TRUE), "CsparseMatrix")

# Build adjacency matrix table (igraph)
# Transition NB list weights to adjacency graph
us_adj <- graph.adjacency(usTemp, mode="undirected")

# Test matrix graph
plot(us_adj)

# Transform to edgelist table (igraph)
# Create edge list with District NCES ID from adjacency graph
us_edge <- get.edgelist(us_adj, names=TRUE)


#######################
# Create Edge Weights #
#######################

###################################
# Union + Elementary Data Created #
# Create data frame from edgelist
ue_dfedge         <- data.frame(ue_edge)
names(ue_dfedge)  <- c("source", "target")
ue_dfedge$id      <- 1:nrow(ue_dfedge)

# Subset source and target nodes to join poverty rate data
ue_source         <- ue_dfedge[c(1,3)]
ue_target         <- ue_dfedge[c(2,3)]

# Subset from data table
names(state_data) <- c("gid", "state.fips", "id.nces", "name", "state.post", "est.pop", "est.student", "est.student.pov", "student.pov.rate")
subset_vars       <- c("id.nces","student.pov.rate")
state_pov         <- state_data[subsetVars]

# Merge poverty data with Source and Target nodes tables
ue_source_data          <- merge(ue_source, state_pov, by.x = "source", by.y="id.nces")
ue_target_data          <- merge(ue_target, state_pov, by.x = "target", by.y="id.nces")
ue_edgeanalysis         <- merge(ue_source_data, ue_target_data, by="id")
names(ue_edgeanalysis)  <- c("id","source","source.pov","target","target.pov")

# Absolute value in difference between Source and Target nodes
ue_edgeanalysis$weight.actual     <- abs(ue_edgeanalysis$source.pov - ue_edgeanalysis$target.pov)
ue_edgeanalysis$weight.adjusted   <- 1 - ue_edgeanalysis$weight.actual        # inverting value of edge weight

###### The latter half of the schema below needs to feature pasting in the defined state code at the top of this script
###### Then the table and suffix will be concatenated to the two letter FIPS code. 
dbWriteTable(con, c("sei",ue.edge.analysis), ue_edgeanalysis, row.names=FALSE) 

# Subset edgeweight analysis table to new data frame for conversion
weight_vars_actual      <- c("source","target","weight.actual")
ue_edge_actual          <- ue_edgeanalysis[weight_vars_actual] # data frame with only source, target, and edge weight

weight_vars_adjusted    <- c("source","target","weight.adjusted")
ue_edge_adj             <- ue_edgeanalysis[weight_vars_adjusted] # data frame with only source, target, and edge weight

# Name state acronym here for writing to RDS
# Datatable with just source, target, and weights for future analysis
dbWriteTable(con, c("sei",ue.edge.actual), ue_edge_actual, row.names=FALSE)
dbWriteTable(con, c("sei",ue.edge.adjusted), ue_edge_adjusted, row.names=FALSE)

##################################
# Union + Secondary Data Created #
# Create data frame from edgelist
us_dfedge           <- data.frame(us_edge)
names(us_dfedge)    <- c("source", "target")
us_dfedge$id        <- 1:nrow(us_dfedge)

# Subset source and target nodes to join poverty rate data
us_source <- us_dfedge[c(1,3)]
us_target <- us_dfedge[c(2,3)]

# Subset from data table
names(state_data) <- c("gid", "state.fips", "id.nces", "name", "state.post", "est.pop", "est.student", "est.student.pov", "student.pov.rate")
subset_vars <- c("id.nces","student.pov.rate")
state_pov <- state_data[subsetVars]

# Merge poverty data with Source and Target nodes tables
us_source_data <- merge(us_source, state_pov, by.x = "source", by.y="id.nces")
us_target_data <- merge(us_target, state_pov, by.x = "target", by.y="id.nces")
us_edgeanalysis <- merge(us_source_data, us_target_data, by="id")
names(us_edgeanalysis) <- c("id","source","source.pov","target","target.pov")

# Absolute value in difference between Source and Target nodes
us_edgeanalysis$weight.actual <- abs(us_edgeanalysis$source.pov - us_edgeanalysis$target.pov)
us_edgeanalysis$weight.adjusted <- 1 - us_edgeanalysis$weight.actual        # inverting value of edge weight

###### The latter half of the schema below needs to feature pasting in the defined state code at the top of this script
###### Then the table and suffix will be concatenated to the two letter FIPS code. 
dbWriteTable(con, c("sei", us.edge.analysis), state_edgeanalysis, row.names=FALSE) 

# Subset edgeweight analysis table to new data frame for conversion
weight_vars_actual <- c("source","target","weight.actual")
us_edge_actual <- us_edgeanalysis[weight_vars_actual] # data frame with only source, target, and edge weight

weight_vars_adjusted <- c("source","target","weight.adjusted")
us_edge_adj <- us_edgeanalysis[weight_vars_adjusted] # data frame with only source, target, and edge weight

# Name state acronym here for writing to RDS
# Datatable with just source, target, and weights for future analysis
dbWriteTable(con, c("sei",us.edge.actual), us_edge_actual, row.names=FALSE)
dbWriteTable(con, c("sei",us.edge.adjusted), us_edge_adjusted, row.names=FALSE)

##################################################################
# Append Actual and Adjusted Edgelists into a Statewide Edgelist #

# Join Unified/Elementary and Unified/Secondary Edgelists together using Smartbind
state_edge_actual <- smartbind(ue_edge_actual, us_edge_actual)
state_edge_adjusted <- smartbind(ue_edge_adjusted, us_edge_adjusted)

# Write appended edgelists to database for further export and further analysis below
dbWriteTable(con, c("sei","state_edge_actual"), state_edge_actual, row.names=FALSE)
dbWriteTable(con, c("sei","state_edge_adjusted"), state_edge_adjusted, row.names=FALSE)

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

# subset qualitative data from state_data for creation of node list

############################
# Network based statistics #
############################

# Node Strength for Outbound Links (even though it's a undirected network) of Actual_Graph
state_node_strength <- graph.strength(state_g_actual, mode="out")
dbWriteTable(con, c("sei",db.node.strength), state_node_strength, row.names=FALSE)

# Identify edges that segregate with threshold factor
# For Dataframe - State Edge Actual
segregated_borders <- state_edge_actual[ which(state_edge_actual$weight > .20), ]           # This row is where the threshold is set - current value set to .2
dbWriteTable(con, c("sei",db.segregated.districts), segregated_borders, row.names=FALSE)

# Identify communities within weighted undirected network
segregated_communities <- cluster_louvain(state_g_actual)
dbWriteTable(con, c("sei",db.communities), segregated_communities, row.names=FALSE)
