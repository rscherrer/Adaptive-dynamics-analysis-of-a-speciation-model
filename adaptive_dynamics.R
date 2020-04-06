rm(list = ls())

# Here we make figures to show the outcome of the adaptive dynamics across parameter
# space, from data generated in Mathematica.

library(tidyverse)
library(cowplot)

# Load the data
data1 <- read.csv("data/adaptive_dynamics_m0.001.csv", header = TRUE)
data2 <- read.csv("data/adaptive_dynamics_m0.01.csv", header = TRUE)
data3 <- read.csv("data/adaptive_dynamics_m0.1.csv", header = TRUE)
data <- rbind(data1, data2, data3)
rm(data1, data2, data3)

# A few corrections
data$isconv <- data$isconv == "True"
data[, c("x", "N1", "N2", "curv")] <- sapply(data[, c("x", "N1", "N2", "curv")], function(x) as.numeric(as.character(x)))
data[, c("a", "b", "h", "s", "d", "m", "alpha")] <- sapply(data[, c("a", "b", "h", "s", "d", "m", "alpha")], round, 3)

head(data)

# Starting trait value (to evaluate whether a branching point can be reached)
x0 <- -1 

# Each observation in the data is a singularity
# Summarize the data per parameter set
smr <- data %>% group_by(a, b, h, s, d, m, alpha) %>% 
  mutate(
    isbp = isconv & curv > 0,
    isabove = x > x0,
    isbelow = x <= x,
    distance = abs(x - x0)
  ) %>%
  summarize(
    
    # Is there any branching point?
    anybp = any(isbp), 
    
    # Find the closest singularities surrounding the starting point
    anyabove = any(isabove), 
    anybelow = any(isbelow),
    iabove = ifelse(anyabove, which(isabove & distance == min(distance[isabove])), NA),
    ibelow = ifelse(anybelow, which(isbelow & distance == min(distance[isbelow])), NA),
    
    # Which singularities are convergence stable?
    isconvabove = ifelse(anyabove, isconv[iabove], NA),
    isconvbelow = ifelse(anybelow, isconv[ibelow], NA),
    ireached = ifelse(anyabove, ifelse(isconvabove, iabove, ifelse(anybelow, ifelse(isconvbelow, ibelow, NA), -Inf)), ifelse(anybelow, ifelse(isconvbelow, ibelow, Inf), NA)),
    
    # Which are reached and are they evolutionarily stable?
    xreached = ifelse(ireached %in% c(NA, -Inf, Inf), ireached, x[ireached]),
    conv = ifelse(ireached %in% c(NA, -Inf, Inf), NA, isconv[ireached]),
    inv = ifelse(ireached %in% c(NA, -Inf, Inf), NA, curv[ireached] > 0)
    
  ) %>%
  select(a, b, h, s, d, m, alpha, anybp, xreached, conv, inv) %>%
  mutate(branching = conv & inv) %>%
  mutate(whatbp = ifelse(anybp, ifelse(branching, "BP reachable", "BP exists"), "No BP"))

# Facet labels
m_labs <- paste("m =", unique(smr$m))
names(m_labs) <- as.character(unique(smr$m))
alpha_labs <- paste("alpha = ", unique(smr$alpha))
names(alpha_labs) <- as.character(unique(smr$alpha))

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

# One patch model

rm(list = ls())
library(tidyverse)

data1 <- read.csv("data/adaptive_dynamics_patch1.csv")
data2 <- read.csv("data/adaptive_dynamics_patch2.csv")

data1$isconv <- data1$isconv == "True"
data1[, c("x", "N", "curv")] <- sapply(data1[, c("x", "N", "curv")], function(x) as.numeric(as.character(x)))
data1[, c("a1", "a2", "b", "s", "d", "alpha")] <- sapply(data1[, c("a1", "a2", "b", "s", "d", "alpha")], round, 3)

data2$isconv <- data2$isconv == "True"
data2[, c("x", "N", "curv")] <- sapply(data2[, c("x", "N", "curv")], function(x) as.numeric(as.character(x)))
data2[, c("a1", "a2", "b", "s", "d", "alpha")] <- sapply(data2[, c("a1", "a2", "b", "s", "d", "alpha")], round, 3)

data1 %>% 
  group_by(a2, s) %>% 
  summarize(anybp = any(isconv & curv > 0)) %>% 
  ggplot(aes(x = a2, y = s, fill = anybp)) + 
  geom_tile() 

data2 %>% 
  group_by(a1, s) %>% 
  summarize(anybp = any(isconv & curv > 0)) %>% 
  ggplot(aes(x = a1, y = s, fill = anybp)) + 
  geom_tile() 

data1 %>% 
  group_by(a2, s) %>% 
  summarize(anybp = any(isconv & curv > 0)) %>% 
  ggplot(aes(x = a2, y = s, fill = anybp)) + 
  geom_tile() 