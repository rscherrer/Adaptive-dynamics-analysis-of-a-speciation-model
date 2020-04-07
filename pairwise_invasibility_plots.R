# Show some pairwise invasibility plots

rm(list = ls())

# Here we produce PIP figures to illustrate the transformation of the branching point
# as the strength of selection changes
# The data were generated in Mathematica

library(tidyverse)
library(AdaptiveDynamicsSpeciation)

data <- do.call("rbind", list(
  read.csv("data/pip1.csv") %>% mutate(s = 0.01),
  read.csv("data/pip2.csv") %>% mutate(s = 0.5),
  read.csv("data/pip3.csv") %>% mutate(s = 1),
  read.csv("data/pip4.csv") %>% mutate(s = 1.5),
  read.csv("data/pip5.csv") %>% mutate(s = 2),
  read.csv("data/pip6.csv") %>% mutate(s = 2.5)
))

s_labels <- make_facet_labels(data, 's')

p <- ggplot(data, aes(x = x, y = y, fill = lambda > 1)) +
  geom_tile() +
  facet_grid(. ~ s, labeller = labeller(s = s_labels)) +
  theme_bw() +
  theme(legend.position = "top") +
  scale_fill_manual(values = c(rgb(0.2, 0.2, 0.2), rgb(0.9, 0.9, 0.9))) +
  scale_x_continuous(breaks = seq(-1, 1, 1)) +
  scale_y_continuous(breaks = seq(-1, 1, 1)) +
  xlab("Resident trait value") +
  ylab("Mutant trait value") +
  labs(fill = "Mutant invades")
p

ggsave("figures/pairwise_invasibility_plots.png", p, width = 7, height = 2.3, dpi = 300)

