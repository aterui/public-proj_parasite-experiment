
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse)


# data load ---------------------------------------------------------------

d	<- read_csv("data_fmt/data_aquarium_fmt.csv")

## Response variables
Y <- matrix(NA, ncol=4, nrow=nrow(d))
Y[,1]	<- ((log(d$W50)-log(d$W0))/50)*100
Y[,2] <- ((log(d$W70)-log(d$W0))/70)*100
CF <- c(scale((d$W0*100)/(d$L0/10)^3))
X <- seq(min(CF), max(CF), length=100)
Ylab <- "SGR"
Mlab <- c("Day 50", "Day 70",NA,NA)
lty <- c(1,1,2,2)

## load coef
coef <- list(NULL)
coef[[1]] <- read.csv("data_fmt/2017-01-18W50re.csv")
coef[[2]] <- read.csv("data_fmt/2017-01-18W70re.csv")


# plot --------------------------------------------------------------------

pdf(file="output/figure_2.pdf",height = 5, width = 8.8)

par(mfrow=c(1,2), mar=c(3,4,1,1), oma=c(3,2,4,3))
for(i in 1:2){
  tmp <- coef[[i]]
  st1 <- which(tmp$X=="Yp[1,1]"); en1 <- which(tmp$X=="Yp[100,1]")
  st2 <- which(tmp$X=="Yp[1,2]"); en2 <- which(tmp$X=="Yp[100,2]")
  
  plot(0, type="n",
       xlim = c(min(CF), max(CF)),
       ylim = c(min(Y[,i]), max(Y[,i])),
       xaxt = "n", yaxt = "n",
       xlab = NA, ylab= NA)
  axis(1); axis(2,las=2)
  mtext(Mlab[i], line=1)
  
  xx <- c(X,rev(X))
  # Control group
  yy <- c(tmp$X2.5.[st1:en1], rev(tmp$X97.5.[st1:en1]))
  polygon(xx, yy, col=rgb(0,0,0,0.5), border=NA)
  lines(X, tmp$X50.[st1:en1], lwd=2, col=rgb(1,1,1),lty=lty[i])
  
  # Treatment group
  yy <- c(tmp$X2.5.[st2:en2], rev(tmp$X97.5.[st2:en2]))
  polygon(xx, yy, col=rgb(0,0,0,0.2), border=NA)
  lines(X, tmp$X50.[st2:en2], lwd=2, col=rgb(0,0,0),lty=lty[i])
  
  # Data points
  points(CF[d$infection==0],Y[,i][d$infection==0], pch=21, bg=rgb(1,1,1), cex=1.2)
  points(CF[d$infection==1],Y[,i][d$infection==1], pch=19, cex=1.2)
}
mtext(Ylab, 2, outer=T) 
mtext("Standardized condition factor", 1, outer=T) 
mtext("Figure 2 Ooue et al.", 3, outer=T, 2) 

dev.off()