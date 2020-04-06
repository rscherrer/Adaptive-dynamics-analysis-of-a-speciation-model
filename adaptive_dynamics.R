rm(list = ls())

# Here we make figures to show the outcome of the adaptive dynamics across parameter
# space, from data generated in Mathematica.

library(tidyverse)
library(cowplot)
library(AdaptiveDynamicsSpeciation)

#### Two-patch model ####

# Load the data
numerics <- c("a", "b", "h", "s", "d", "m", "alpha", "x", "N1", "N2", "curv")
data <- do.call("rbind", list(
  read.csv("data/adaptive_dynamics_m0.001.csv", header = TRUE) %>% mathematica2r(numerics = numerics),
  read.csv("data/adaptive_dynamics_m0.01.csv", header = TRUE) %>% mathematica2r(numerics = numerics),
  read.csv("data/adaptive_dynamics_m0.1.csv", header = TRUE) %>% mathematica2r(numerics = numerics)
))

# Each observation in the data is a singularity
# Summarize the data per parameter set
smr <- data %>% group_by(a, b, h, s, d, m, alpha) %>%
  predict_adaptive_dynamics(xstart = -1) %>%
  select(a, b, h, s, d, m, alpha, anybp, xreached, conv, inv) %>%
  mutate(branching = conv & inv)

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
  scale_fill_manual(values = c(rgb(0.8, 0.8, 0.8), rgb(0.7, 0.7, 0.7), rgb(0.6, 0.6, 0.6))) +
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
  scale_fill_manual(values = c(rgb(0.8, 0.8, 0.8), rgb(0.7, 0.7, 0.7), rgb(0.6, 0.6, 0.6))) +
  xlab("Habitat symmetry") +
  ylab("Ecological divergent selection") +
  labs(fill = "Sexual\nselection")
p

# Summarize the relevant information into a nice figure
p <- p + geom_line(data = smr. %>% group_by(h, m) %>% summarize(s = max(s)), aes(x = h, y = s, fill = NULL), lty = 2) +
  geom_line(data = smr. %>% filter(alpha == 1) %>% group_by(h, m) %>% summarize(s = max(s)), aes(x = h, y = s, fill = NULL), lty = 3) +
  geom_line(data = smr. %>% filter(alpha == 0) %>% group_by(h, m) %>% summarize(s = max(s)), aes(x = h, y = s, fill = NULL), lty = 4) +
  geom_line(data = smr. %>% filter(alpha == 0) %>% group_by(h, m) %>% summarize(s = min(s)), aes(x = h, y = s, fill = NULL), lty = 5)
p  

ggsave("figures/map_branching_points.png", p, width = 7, height = 3, dpi = 300)




################

#### One-patch model ####

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

