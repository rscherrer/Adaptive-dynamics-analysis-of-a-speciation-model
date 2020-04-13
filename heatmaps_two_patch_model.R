# Map the adaptive dynamics of the two-patch model

rm(list = ls())

# Here we produce maps showing where branching occurs across parameter space
# The maps are split in several values of the dispersal rate
# For each map we plot in shades of grey the realized branching when starting with an initial trait value of -1
# And we add lines to show the domain where branching can occur in theory, but is not necessarily reachable
# Sexual selection changes the stability of the branching point, so we overlay the figures for different
# intensities of sexual selection (if branching occurs at a high level of sexual selection, it also occurs 
# at lower levels)
# The data were generated in Mathematica

library(tidyverse)
library(cowplot)
library(AdaptiveDynamicsSpeciation)

# Load the data
numerics <- c("a", "b", "h", "s", "d", "m", "alpha", "x", "conv", "inv", "n1", "n2")
data <- do.call("rbind", list(
  read.csv("data/adaptive_dynamics_m_0.001.csv", header = TRUE),
  read.csv("data/adaptive_dynamics_m_0.01.csv", header = TRUE),
  read.csv("data/adaptive_dynamics_m_0.1.csv", header = TRUE)
)) %>% mathematica2r(numerics = numerics)

# Each observation in the data is a singularity
# Summarize the data per parameter set
smr <- data %>% group_by(a, b, h, s, d, m, alpha) %>%
  predict_adaptive_dynamics(xstart = -1) %>%
  select(a, b, h, s, d, m, alpha, anybp, xreached, convergent, invasible) %>%
  mutate(branching = convergent & invasible)

# Facet labels
m_labs <- make_facet_labels(smr, "m")
alpha_labs <- make_facet_labels(smr, "alpha")

# Is there any branching point?
p <- ggplot(smr %>% filter(anybp), aes(x = h, y = s, fill = anybp)) + 
  geom_tile() + 
  facet_grid(m ~ alpha, labeller = labeller(m = m_labs, alpha = alpha_labs)) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab("Habitat symmetry") +
  ylab("Ecological divergent selection") +
  scale_fill_manual(values = rgb(0.7, 0.7, 0.7))
p

# What is the maximum sexual selection value at which branching can still occur?
smr. <- smr %>% group_by(a, b, h, s, d, m) %>%
  summarize(alpha = max(alpha[anybp])) %>%
  mutate(alpha = replace(alpha, alpha == -Inf, NA)) %>%
  filter(!is.na(alpha))

p <- ggplot(smr., aes(x = h, y = s, fill = as.factor(alpha))) + 
  geom_tile() +
  facet_grid(. ~ m, labeller = labeller(m = m_labs)) +
  theme_bw() +
  scale_fill_manual(values = c(rgb(0.8, 0.8, 0.8), rgb(0.7, 0.7, 0.7), rgb(0.6, 0.6, 0.6), rgb(0.5, 0.5, 0.5))) +
  xlab("Habitat symmetry") +
  ylab("Ecological divergent selection") +
  labs(fill = "Sexual\nselection")
p

# Is there a branching point reachable from the starting point?
p <- ggplot(smr %>% filter(branching), aes(x = h, y = s, fill = branching)) +
  geom_tile() +
  facet_grid(m ~ alpha, labeller = labeller(m = m_labs, alpha = alpha_labs)) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab("Habitat symmetry") +
  ylab("Ecological divergent selection") +
  scale_fill_manual(values = rgb(0.7, 0.7, 0.7))
p

# What is the maximum sexual selection value at which branching can still be reached from the starting point?
smr.. <- smr %>% filter(branching) %>% group_by(a, b, h, s, d, m) %>%
  summarize(alpha = max(alpha[anybp])) %>%
  mutate(alpha = replace(alpha, alpha == -Inf, NA)) %>%
  filter(!is.na(alpha))

p <- ggplot(smr.., aes(x = h, y = s, fill = as.factor(alpha))) +
  geom_tile() +
  facet_grid(. ~ m, labeller = labeller(m = m_labs)) +
  theme_bw() +
  scale_fill_manual(values = c(rgb(0.8, 0.8, 0.8), rgb(0.7, 0.7, 0.7), rgb(0.6, 0.6, 0.6), rgb(0.5, 0.5, 0.5))) +
  xlab("Habitat symmetry") +
  ylab("Ecological divergent selection") +
  labs(fill = "Sexual\nselection") +
  ylim(c(0, 2.5))
p

# Summarize the relevant information into a nice figure
p <- p + geom_line(data = smr. %>% group_by(h, m) %>% summarize(s = max(s)), aes(x = h, y = s, fill = NULL), lty = 2) +
  geom_line(data = smr. %>% filter(alpha == 10) %>% group_by(h, m) %>% summarize(s = max(s)), aes(x = h, y = s, fill = NULL), lty = 3) +
  geom_line(data = smr. %>% filter(alpha == 1) %>% group_by(h, m) %>% summarize(s = max(s)), aes(x = h, y = s, fill = NULL), lty = 4) +
  geom_line(data = smr. %>% filter(alpha == 0) %>% group_by(h, m) %>% summarize(s = max(s)), aes(x = h, y = s, fill = NULL), lty = 5) +
  geom_line(data = smr. %>% filter(alpha == 0) %>% group_by(h, m) %>% summarize(s = min(s)), aes(x = h, y = s, fill = NULL), lty = 6)
p  

ggsave("figures/map_branching_points.png", p, width = 7, height = 3, dpi = 300)

