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
