# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse,
               rvg)

# data load ---------------------------------------------------------------

d	<- read_csv("data_fmt/data_aquarium_fmt.csv")


# plot --------------------------------------------------------------------

pdf(file="output/figure_1.pdf", 8, 7)

par(mfrow=c(2,2), mar=c(3,5,3,3), oma=c(2,2,2,1))
## FL ##
  xmax <- 110; xmin <- 65; ymax <- 18
  BR <- seq(xmin,xmax,2)
  hist(d$L0[d$treatment=="C"], xlim=c(xmin,xmax),
       axes=F, ann=F, ylim=c(0,ymax), breaks=BR)
  par(new=T)
  hist(d$L50[d$treatment=="C"], xlim=c(xmin,xmax),
       axes=F, ann=F, ylim=c(0,ymax), breaks=BR, col=rgb(0,0,0,0.2))
  par(new=T)
  hist(d$L70[d$treatment=="C"], xlim=c(xmin,xmax),
       axes=F, ann=F, ylim=c(0,ymax), breaks=BR, col=rgb(1,0,0,0.2))
  box(bty="l")
  axis(1); axis(2,las=2)
  mtext("Fork length (mm)", 1, line=3)
  mtext("(a) Uninfected", 3)
  
  # parasite treatment
  hist(d$L0[d$treatment=="P"], xlim=c(xmin,xmax),
       axes=F, ann=F, ylim=c(0,ymax), breaks=BR)
  par(new=T)
  hist(d$L50[d$treatment=="P"], xlim=c(xmin,xmax),
       axes=F, ann=F, ylim=c(0,ymax), breaks=BR, col=rgb(0,0,0,0.2))
  par(new=T)
  hist(d$L70[d$treatment=="P"], xlim=c(xmin,xmax),
       axes=F, ann=F, ylim=c(0,ymax), breaks=BR, col=rgb(1,0,0,0.2))
  box(bty="l")
  axis(1); axis(2,las=2)
  mtext("Fork length (mm)", 1, line=3)
  mtext("(b) Infected", 3)
  
## wet mass##
  xmax <- 16; xmin <- 0; ymax <- 25
  BR <- seq(xmin,xmax,1)
  hist(d$W0[d$treatment=="C"], xlim=c(xmin,xmax),
       axes=F, ann=F, ylim=c(0,ymax), breaks=BR)
  par(new=T)
  hist(d$W50[d$treatment=="C"], xlim=c(xmin,xmax),
       axes=F, ann=F, ylim=c(0,ymax), breaks=BR, col=rgb(0,0,0,0.2))
  par(new=T)
  hist(d$W70[d$treatment=="C"], xlim=c(xmin,xmax),
       axes=F, ann=F, ylim=c(0,ymax), breaks=BR, col=rgb(1,0,0,0.2))
  box(bty="l")
  axis(1); axis(2,las=2)
  mtext("Wet mass (g)", 1, line=3)
  
  # parasite treatment
  hist(d$W0[d$treatment=="P"], xlim=c(xmin,xmax),
       axes=F, ann=F, ylim=c(0,ymax), breaks=BR)
  par(new=T)
  hist(d$W50[d$treatment=="P"], xlim=c(xmin,xmax),
       axes=F, ann=F, ylim=c(0,ymax), breaks=BR, col=rgb(0,0,0,0.2))
  par(new=T)
  hist(d$W70[d$treatment=="P"], xlim=c(xmin,xmax),
       axes=F, ann=F, ylim=c(0,ymax), breaks=BR, col=rgb(1,0,0,0.2))
  box(bty="l")
  axis(1); axis(2,las=2)
  mtext("Wet mass (g)", 1, line=3)
mtext("Frequency", 2, outer=T)
mtext("Figure 1 Ooue et al.", 3, outer=T)

dev.off()