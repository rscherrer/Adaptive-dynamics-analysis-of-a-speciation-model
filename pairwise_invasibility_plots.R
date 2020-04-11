# Show some pairwise invasibility plots

rm(list = ls())

# Here we produce PIP figures to illustrate the transformation of the branching point
# as the strength of selection changes
# The data were generated in Mathematica

library(tidyverse)
library(AdaptiveDynamicsSpeciation)

data <- do.call("rbind", list(
  read.csv("data/pairwise_invasibility_plot_s_0.01.csv") %>% mutate(s = 0.01),
  read.csv("data/pairwise_invasibility_plot_s_0.5.csv") %>% mutate(s = 0.5),
  read.csv("data/pairwise_invasibility_plot_s_1.csv") %>% mutate(s = 1),
  read.csv("data/pairwise_invasibility_plot_s_1.5.csv") %>% mutate(s = 1.5),
  read.csv("data/pairwise_invasibility_plot_s_2.csv") %>% mutate(s = 2),
  read.csv("data/pairwise_invasibility_plot_s_2.5.csv") %>% mutate(s = 2.5)
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

ggsave("figures/pairwise_invasibility_plots_test_s.png", p, width = 7, height = 2.3, dpi = 300)

#####

data <- do.call("rbind", list(
  read.csv("data/pairwise_invasibility_plot_alpha_0.csv") %>% mutate(alpha = 0),
  read.csv("data/pairwise_invasibility_plot_alpha_1.csv") %>% mutate(alpha = 1),
  read.csv("data/pairwise_invasibility_plot_alpha_10.csv") %>% mutate(alpha = 10),
  read.csv("data/pairwise_invasibility_plot_alpha_100.csv") %>% mutate(alpha = 100)
))

alpha_labels <- gsub("alpha", "\U03B1", make_facet_labels(data, 'alpha'))

p <- ggplot(data, aes(x = x, y = y, fill = lambda > 1)) +
  geom_tile() +
  facet_grid(. ~ alpha, labeller = labeller(alpha = alpha_labels)) +
  theme_bw() +
  theme(legend.position = "top") +
  scale_fill_manual(values = c(rgb(0.2, 0.2, 0.2), rgb(0.9, 0.9, 0.9))) +
  scale_x_continuous(breaks = seq(-1, 1, 1)) +
  scale_y_continuous(breaks = seq(-1, 1, 1)) +
  xlab("Resident trait value") +
  ylab("Mutant trait value") +
  labs(fill = "Mutant invades")
p

ggsave("figures/pairwise_invasibility_plots_test_alpha.png", p, width = 5, height = 2.3, dpi = 300)
