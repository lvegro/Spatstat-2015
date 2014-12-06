for (i in 1:5){
  t <- F[[i]]
  
d1 <- t
geot <- as.geodata(t, coords.col=1:2, data.col=4) # con il logar. col 4

require(sp)
require(spatstat)
require(gstat)


coordinates(d1) <- ~x+y

w <- convexhull.xy(t$x, t$y)
pattern<- as.ppp(X=(cbind(t$x, t$y)), W=w)

dlim<- range(pairdist.ppp(pattern))
hscat(log.fe~1,d1,breaks=seq(dlim[1], dlim[2]/2,length.out=14))
md <- dlim[2]
}