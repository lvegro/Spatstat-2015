### Ferro - ESDA Larga Scala
## I dati sono contenuti in una lista di nome F, dove ognuno dei 5 elementi è un df
## con attributi x, y, fe, e log.fe.

# 
# lapply(F,
#        function(F){
#          

test<-F[[1]]


require(spatstat)
w <- convexhull.xy(test$x, test$y)
pp <- ppp(test$x, test$y, window=w, marks=test$log.fe)
# grafico a barre delle distanze dal primo vicino più prossimo
hist(nndist(pp), col="grey75", breaks=15, xlab="distanza", ylab="frequenza", main="Distanza dal primo vicino")

# log-concentrazione in funzione di longitudine e latitudine
# con linee di tendenza via kernel
par(mfrow = c(1,2))
lapply(F, function(test){
with(test, {plot(log.fe ~ x,
                xlab="longitudine",
                ylab="log conc",
                main="logc vs long",
                col="grey80",
                pch=19,
                cex=.6)
                lines(ksmooth(x, log.fe,
                              "normal",
                              bandwidth=sd(test$x)),
                      col=2, lwd=2.5)})
with(test, {plot(log.fe ~ y,
                xlab="latitudine",
                ylab="log conc",
                main="logc vs lat",
                col="grey80",
                pch=19,
                cex=.6)
     lines(ksmooth(y, log.fe,
                   "normal",
                   bandwidth=sd(test$y)),
           col=2, lwd=2.5)})
})


### spatial outlier
### procedimento
# determina  matrice di prossimità
# da matrice di prossimità a pesi
# standardizza logfe
# calcola local I per tutti i pti
