
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse,
               runjags)

source("function_jags2bugs.R")

# data --------------------------------------------------------------------

d	<- read_csv("data_fmt/data_aquarium_fmt.csv")

## Response variables
Y <- matrix(NA, ncol=4, nrow=nrow(d))
Y[,1]	<- ((log(d$L50)-log(d$L0))/50)*100
Y[,2]	<- ((log(d$L70)-log(d$L0))/70)*100
Y[,3]	<- ((log(d$W50)-log(d$W0))/50)*100
Y[,4] <- ((log(d$W70)-log(d$W0))/70)*100

r_name <- c("L50","L70","W50", "W70")

## Explanatory variables
CF <- c(scale((d$W0*100)/(d$L0/10)^3))
X <- seq(min(CF), max(CF), length=100)
inf <- c(rep(1,5), rep(0,5))# 1:5 infected treatment, 6:10 non-infected treatment

## Set-up block identity
block <-	NULL
names <-	paste(d$treatment, d$aquarium, sep=""); NAMES <- unique(names)
for(i in 1:length(NAMES)){ block[names==NAMES[i]] <- i }

## Number of samples
Nind <- nrow(d)
Nblock <- length(unique(block))


# parameters for jags -----------------------------------------------------

# MCMC setting
n.ad <- 100
n.iter <- 5E+3
n.thin <- max(3, ceiling(n.iter/500))
burn <- ceiling(max(10, n.iter/2))
Sample <- ceiling(n.iter/n.thin)

# initial values
inits <- replicate(3,
                   list(.RNG.name="base::Wichmann-Hill", .RNG.seed=round(runif(1,1,100))),
                   simplify = FALSE)

# run model
bpost <- list(NULL)
para <- c("Yp", "b", "beta", "sigma")

for(i in 1:4){
  Djags <- list(y = Y[,i], x = CF, X = X, block=block, inf = inf,
                Nind = Nind, Nblock = Nblock)
  m <- read.jagsfile("model_aqua.R")
  post <- run.jags(m$model, monitor = para, data = Djags,
                   n.chains = 3, inits=inits, method = "parallel",
                   burnin = burn, sample = Sample, adapt = n.ad, thin = n.thin,
                   n.sims = 3)
  
  bpost[[i]] <- jags2bugs(post$mcmc)
}

# Save results
#filename <- tmp <- NULL
#for(i in 1:4){
#  filename[i] <- paste("re_revise1/",Sys.Date(), r_name[i],"re", ".csv", sep="")
#  write.csv(bpost[[i]]$summary, filename[i])
#}
