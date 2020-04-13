# Map the adaptive dynamics of the one-patch model

rm(list = ls())

# Here we produce figures showing the extent of phenotypic divergence between
# two habitats analyzed independently, i.e. with no gene flow between them
# We add an extra layer showing the parameter space where branching occurs 
# within habitats. If the habitats are mirror-images of each other, if branching
# occurs in one, it also occurs in the other. 
# We compare situations with starting trait values 0 and -1

library(tidyverse)
library(cowplot)
library(AdaptiveDynamicsSpeciation)

numerics <- c("a", "h", "b", "s", "d", "alpha", "x", "conv", "inv", "n")
data <- do.call(rbind, list(
  read.csv("data/adaptive_dynamics_patch1.csv") %>% mutate(patch = 1),
  read.csv("data/adaptive_dynamics_patch2.csv") %>% mutate(patch = 2)
)) %>% mathematica2r(numerics = numerics) 

# Summarize and make a nice figure
p1 <- plot_divergence_map(data, xstart = 0)
leg <- get_legend(p1)
p1 <- p1 + theme(legend.position = "none")
p2 <- plot_divergence_map(data, xstart = -1) + theme(legend.position = "none")
p <- plot_grid(plot_grid(p1, p2, labels = c("A", "B")), leg, nrow = 2, rel_heights = c(5, 1))
p 

ggsave("figures/divergence_across_patches.png", p, width = 7, height = 4, dpi = 300)

