# carica i pacchetti richiesti
require(reshape2)
require(sp)
require(spatstat)
require(maptools)
require(RColorBrewer)

pal <- brewer.pal(6, "Accent")

# carica i dati dal csv
# setwd("C:/Users/Lorenzo/Desktop/spaziale")
tmp <- read.csv(file="FE.csv", header=T, sep=";")

# anno è categoriale
tmp$anno <- as.factor(tmp$anno)

# c'è un outlier in 2011, lo metto NA
tmp$ferro[which.max(tmp$ferro)]<- NA

# rimuove gli elementi duplicati
# ci sono alcuni elementi che si ripetono esattamente: dovrebbero essere errori
# dal momento che è decisamente difficile ottenere la stessa misura anche
# ricampionando sullo stesso sito nello stesso momento

# "allargo" il data set con una colonna per ogni anno: dcast è in reshape2
# in questo caso aggrego le rilevazioni sullo stesso sito con la media
# (sarebbe forse meglio la mediana?)
wide <- dcast(data=tmp,
              formula = CODICE + X_WGS84 + Y_WGS84~ anno,
              fun.aggregate=mean,
              value.var="ferro" )

## Mappa Lombardia e finestra del processo
map <- readShapePoly("shp/RLGeoDATA//Sfondo_polygon.shp")
w <- as.owin(map)

# rinomino le colonne di wide: il vettore di stringhe a dx sovrascrive quello a
# sx
names(wide) <- c("cod",
                 "x",
                 "y",
                 "fe.09",
                 "fe.10",
                 "fe.11",
                 "fe.12",
                 "fe.13")

# prealloca una lista di 5 elementi
F <- vector("list", 5)
# popola la lista con 5 dataframe ciascuno con X=long, Y=lat, Fe=conc. ferro
# nb: ogni attributo di ogni elemento della lista ha lunghezza di 381 = # siti

for(i in 1:5){
  F[[i]] <- data.frame(wide$x,
                       wide$y,
                       wide[,3+i],
                       log(wide[,3+i]))
  names(F[[i]]) <- c("x", "y", "fe", "log.fe")
  F[[i]] <- F[[i]][complete.cases(F[[i]]),] # solo casi completi
}
# pulisci lo spazio
rm (tmp, i)
