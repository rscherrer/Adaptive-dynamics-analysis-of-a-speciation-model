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

numerics <- c("a1", "a2", "b", "s", "d", "alpha", "x", "N", "curv")
data <- do.call(rbind, list(
  read.csv("data/adaptive_dynamics_patch1.csv") %>% mathematica2r(numerics = numerics) %>% mutate(patch = 1),
  read.csv("data/adaptive_dynamics_patch2.csv") %>% mathematica2r(numerics = numerics) %>% mutate(patch = 2)
)) %>%
  
  # Sexual selection has no influence on the results here (probably can prove that)
  filter(alpha == 0) %>%
  
  # Evaluate habitat symmetry
  mutate(a = max(a1, a2), h = a1 / a2) %>%
  mutate(h = replace(h, h > 1, (a2 / a1)[h > 1]))

p1 <- plot_divergence_map(data, xstart = 0, keep_legend = FALSE)
p2 <- plot_divergence_map(data, xstart = -1)
p <- plot_grid(p1, p2, labels = c("A", "B"), rel_widths = c(0.8, 1.2))
p 

ggsave("figures/divergence_across_patches.png", p, width = 7, height = 3, dpi = 300)

