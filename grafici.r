require(ggplot2)

theme_jack <- function (base_size = 12, base_family = "") {
  theme_gray(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      axis.text = element_text(colour = "black"),
      axis.title.x = element_text(colour = "Black"),
      axis.title.y = element_text(colour = "Black", angle=45),
      panel.background = element_rect(fill="gray95"),
      panel.grid.minor.y = element_line(size=0),
      panel.grid.major = element_line(colour = "white"),
      plot.background = element_rect(fill="white")
    )   
}

theme_set(theme_jack())

### Produzione Grafici
# istogrammi


pdf("grafica\\istogrammi.pdf")
lapply(F, function(x){
  hist(x =  x$log.fe,
        ylab = "Frequenza",
        xlab = "log(Concentrazione)",breaks=15
  )})
dev.off()

# qplot
i <- 1
lapply(F, function(x){
  ggplot(data=x,
       aes(x = log.fe)) +
    geom_histogram(aes(y=..density..),colour="black", fill="gray80", binwidth=0.75) +
    stat_function(fun = dnorm,
                  colour="black",
                  linetype='dashed',
                  size=1.1,
                  args =list(mean = mean(x$log.fe), sd = sd(x$log.fe))) +
    ylab("Frequenza") +
    xlab("log(Concentrazione)")
  })
