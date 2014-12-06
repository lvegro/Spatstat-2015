# per le prove
i <- 1
require(geoR)
#for (i in 1:5){

t <- F[[i]]

# rendilo geodata
# d1 è un df (x,y, data) a cui sovraimponiamo le coordinate per costruire gli scatter
# laggati
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

# ### nuvola del variogramma
par(mfrow=c(2,1)) 
plot(variog(geot, estimator.type="classical",option="cloud"),cex=0.4, max.dist=md) #variogramma cloud 
plot(variog(geot, estimator.type="modulus",option="cloud"),cex=0.4, max.dist=md) 
# 
# # variog binnato
# 
# par(mfrow=c(2,1)) 
# bin1<-variog(geot, estimator.type="classical",option="bin", bin.cloud=TRUE,max.dist=md) 
# # ragruppa i punti della nuvola del variogramma in gruppi per produrne poi i boxplot 
# bin2<-variog(geot, estimator.type="modulus",option="bin", bin.cloud=TRUE, max.dist=md) 
# # come sopra ma usando la trasformata |Y(s)-Y(r)|^1/2 
# 
# 
# vario.0 <- variog(geot, dir=0, tol=pi/8) 
# vario.45 <- variog(geot, dir=pi/4, tol=pi/8) 
# vario.90 <- variog(geot, dir=pi/2, tol=pi/8) 
# vario.135 <- variog(geot, dir=pi*3/4, tol=pi/8) 
# 
# par(mfrow=c(1,1)) # ripristina una finestra per il grafico
# 
# plot(c(0,md?),c(0,9),type="n",xlab="distanza",ylab="semivarianza")#,type="p" 
# lines(bin1) 
# lines(vario.0,col=2,lty=2) 
# lines(vario.45,col=3,lty=3) 
# lines(vario.90,col=4,lty=4) 
# lines(vario.135,col=5,lty=5) 
# 
# legend(10,100, legend=c("omnidirectional", expression(0 * degree), 
#                         expression(45 * degree), expression(90 * degree), 
#                         expression(135 * degree)), lty=c(1:5),col=1:5) 
# #}
# 
# ### stima parametrica del variogramma
# 
variogramma <- variog(geot, estimator.type="classical", 
                      max.dist=md/2) # variogramma empirico 
# plot(variogramma, main="valutazione visiva variogrammi") 
# lines.variomodel(cov.model = "exp", cov.pars = c(25,35), nug =2.5, 
#                  max.dist=md/2,col=2,lty=2) # aggiunge plot 'teorico' con valori fissati 
# 
# # #--- VERIFICARE USANDO L'HELP DELLA FUNZIONE L'USO DI lines.variomodel 
# # lines.variomodel(cov.model = "exp", cov.pars = c(20,35), nug =20, 
# #                  max.dist=max.dist,col=3,lty=3) # aggiunge curva “teorica” con valori fissati 
# # lines.variomodel(cov.model = "exp", cov.pars = c(30,35), nug =20, 
# #                  max.dist=max.dist,col=4,lty=4) 
# # legend(70,30,c("variogramma empirico","soglia parziale 25","soglia parziale 20","soglia parziale 
# # 30"),col=1:4,lty=1:4) 
# # #--- provare valutazioni
# 
exp.fit <- variofit(variogramma, ini.cov.pars=c(3,md/5), 
                           cov.model="exponential", fix.nugget=FALSE,nugget=2) 
summary(dat.om.var.fit) 
dat.om.var.fit$cov.pars
plot(variogramma)
lines(exp.fit)

#}