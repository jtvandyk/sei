# NJ = data table
# NJ.nb = neighbors list
# NJ.lw = list weights
# NJ.sp = spatial polygon

# Load data table

#Extract coordinates from spatial coordinates
NJ.coords <- coordinates(NJ.sp)

# Plot neighbor connections
par(mar=rep(0,4))
plot(NJ.lw,coords=NJ.coords,pch=19, cex=0.1, col="gray")

# Global Autocorrelation Tests: Moran's I
moran.test(NJ$EstPovRate, listw=NJ.lw, zero.policy=T)


# Global Autocorrelation Tests: Geary's C
geary.test(NJ$EstPovRate, listw=NJ.lw, zero.policy=T)

# Moran Scatterplot
par(mar=c(4,4,1.5,0.5))
moran.plot(NJ$EstPovRate, listw=NJ.lw, zero.policy=T, xlim=c(0,100),ylim=c(0,100), pch=16, col="black",cex=.5, quiet=F, labels=as.character(data$NAME),xlab="Percent for Bush", ylab="Percent for Bush (Spatial Lag)", main="Moran Scatterplot")

