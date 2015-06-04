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

########################
#Global Autocorrelation#
########################

# Global Autocorrelation Tests: Moran's I
moran.test(NJ$EstPovRate, listw=NJ.lw, zero.policy=T)


# Global Autocorrelation Tests: Geary's C
geary.test(NJ$EstPovRate, listw=NJ.lw, zero.policy=T)

# Moran Scatterplot
par(mar=c(4,4,1.5,0.5))
moran.plot(NJ$EstPovRate, listw=NJ.lw, zero.policy=T, xlim=c(0,100),ylim=c(0,100), pch=16, col="black",cex=.5, quiet=F, labels=as.character(data$NAME),xlab="Percent for Bush", ylab="Percent for Bush (Spatial Lag)", main="Moran Scatterplot")

########################
# Local Autocorrelation#
########################

#Local Moran's I (normality assumption)
lm1 <- localmoran(NJ$EstPovRate, listw=NJ.lw, zero.policy=T)

NJ$lm1 <- abs(lm1[,4]) ## Extract z-scores

lm.palette <- colorRampPalette(c("white","orange", "red"), space = "rgb")
spplot(NJ, zcol="lm1", col.regions=lm.palette(20), main="Local Moran's I (|z| scores)", pretty=T)

#############################
## Simulated Autocorrelation#
#############################

###########
#SET UP FOR SEI VARIABLES AND TABLE NAMES#
###########

weights <- W_cont_el_mat
n <- length(W_cont_el)
uncorr_x <- rnorm(n)
rho1 <- 0.9
rho2 <- -0.9
autocorr_x <- invIrW(weights, rho1) %*% uncorr_x
ncorr_x <- invIrW(weights, rho2) %*% uncorr_x
w.uncorr_x <- lag(weights, uncorr_x, zero.policy=T, NAOK=T)
w.autocorr_x <- lag(weights, autocorr_x, zero.policy=T, NAOK=T)
w.ncorr_x <- lag(weights, ncorr_x, zero.policy=T, NAOK=T)


## Plot observed vs. lagged values

par(mar=c(4,4,2.5,.5))
plot(uncorr_x, w.uncorr_x, xlab="Y ~ N(0, 1)", ylab="Spatial Lag of Y", main=expression(symbol("\162") == 0),col="grey",cex=.5,xlim=c(-4,4),ylim=c(-4,4))
abline(a=0,b=1,lty="dotted")
lines(lowess(uncorr_x, w.uncorr_x), lty=2, lwd=2, col="red")
legend(x="bottomright", lty=2, lwd=2, col="red", legend="LOESS Curve", bty="n")
## Lowess curve

par(mar=c(4,4,2.5,.5))                 
plot(autocorr_x, w.autocorr_x, xlab="Y ~ N(0, 1)", ylab="Spatial Lag of Y", main=expression(symbol("\162") == 0.9),col="grey",cex=.5,xlim=c(-4,4),ylim=c(-4,4))
abline(a=0,b=1,lty="dotted")
lines(lowess(autocorr_x, w.autocorr_x), lty=2, lwd=2, col="red")

par(mar=c(4,4,2.5,.5))
plot(ncorr_x, w.ncorr_x, xlab="Y ~ N(0, 1)", ylab="Spatial Lag of Y", main=expression(symbol("\162") == -0.9), ,xlim=c(-4,4),ylim=c(-4,4),col="grey",cex=.5)
abline(a=0,b=1,lty="dotted")
lines(lowess(ncorr_x, w.ncorr_x), lty=2, lwd=2, col="red")

dev.off()


