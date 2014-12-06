library(spdep)
### local moran for point pattern
### p.es 2010
### spdep richiede che l'oggetto sia in formato spatialpoints
p2009 <- data.frame(x=F[[2]][,1], y=F[[2]][,2], log.fe=scale(F[[2]]$log.fe))
coordinates(p2009) <- ~x+y
# OK!
nb2listw(knn2nb(knn=knearneigh(x=p2009, k=8))) -> lw
pvals <- localmoran(x=p2009$log.fe, listw=lw)[,5]
p2009$outl <- cut(pvals,breaks=c(0, 0.05, +Inf), labels=c("Outlier", "0"))
palette(c("red", "grey80"))
