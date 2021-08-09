
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse,
               lme4)
source("function_predict_r.R")


# data --------------------------------------------------------------------

df_aqua <- read_csv("data_fmt/data_aquarium_fmt.csv") %>% 
 mutate(condition_factor = W0 * 100 / L0^3,
        block = paste0(treatment, aquarium),
        scl_cf = c(scale(condition_factor)))


# prediction --------------------------------------------------------------

m <- glmer(dominance ~ scl_cf + (1 | block),
           family = binomial,
           data = df_aqua)
fit <- predictR(m,
                f_var = "scl_cf")


# plot --------------------------------------------------------------------

pdf(file="output/figure_s2.pdf", 6, 6)

par(mar=c(5,6,4,3))
plot(dominance ~ scl_cf,
     data = df_aqua,
     axes = F,
     ann = F,
     cex = 2,
     pch = 21,
     col = NA,
     bg = rgb(0,0,0,0.3))

axis(1)
axis(2,
     at = seq(0, 1, length = 5),
     las = 2)

box(bty = "l")
mtext("Initial condition factor K", 1, line = 3)
mtext("Probability of being dominant", 2, line = 4)
LTY <- c(1,2,2)
for(i in 1:3){ lines(fit[,"scl_cf"], fit[,i], lty=LTY[i]) }

dev.off()