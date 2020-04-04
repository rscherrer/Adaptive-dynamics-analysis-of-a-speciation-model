rm(list = ls())

library(tidyverse)

# Import data
data1 <- read.csv("data/adaptive_dynamics_m0.001.csv", header = TRUE)
data2 <- read.csv("data/adaptive_dynamics_m0.01.csv", header = TRUE)
data3 <- read.csv("data/adaptive_dynamics_m0.1.csv", header = TRUE)
data <- rbind(data1, data2, data3)
rm(data1, data2, data3)

data$isconv <- data$isconv == "True"
data[, c("x", "N1", "N2", "curv")] <- sapply(data[, c("x", "N1", "N2", "curv")], function(x) as.numeric(as.character(x)))
data[, c("a", "b", "h", "s", "d", "m", "alpha")] <- sapply(data[, c("a", "b", "h", "s", "d", "m", "alpha")], round, 3)

head(data)

# Summary per parameter set
smr1 <- data %>% group_by(a, b, h, s, d, m, alpha) %>% 
  summarize(anybp = any(isconv & curv > 0))

# Facet labels
m_labs <- paste("m =", unique(smr1$m))
names(m_labs) <- as.character(unique(smr1$m))
alpha_labs <- paste("alpha = ", unique(smr1$alpha))
names(alpha_labs) <- as.character(unique(smr1$alpha))

# Branching points through parameter space
p <- ggplot(smr1 %>% filter(anybp), aes(x = h, y = s, fill = anybp)) + 
  geom_tile() + 
  facet_grid(m ~ alpha, labeller = labeller(m = m_labs, alpha = alpha_labs)) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab("Habitat symmetry") +
  ylab("Ecological divergent selection") +
  scale_fill_manual(values = rgb(0.7, 0.7, 0.7))
p

ggsave("figures/branching_points_large.png", p, width = 7, height = 7)

# Condensed version
smr2 <- smr1 %>% group_by(a, b, h, s, d, m) %>%
  summarize(alpha = max(alpha[anybp]))
smr2$alpha[smr2$alpha == -Inf] <- NA

p <- ggplot(smr2 %>% filter(!is.na(alpha)), aes(x = h, y = s, fill = as.factor(alpha))) + 
  geom_tile() +
  facet_grid(. ~ m, labeller = labeller(m = m_labs)) +
  theme_bw() +
  scale_fill_manual(values = c(rgb(0.8, 0.8, 0.8), rgb(0.7, 0.7, 0.7), rgb(0.6, 0.6, 0.6))) +
  xlab("Habitat symmetry") +
  ylab("Ecological divergent selection") +
  labs(fill = "Sexual\nselection")

p

ggsave("figures/branching_points.png", p, height = 3, width = 7)

# Is the branching point reached from my starting point?
x0 <- -1
smr3 <- data %>% group_by(a, b, h, s, d, m, alpha) %>%
  mutate(
    isabove = x > x0,
    isbelow = x <= x,
    distance = abs(x - x0)
  ) %>%
  summarize(
    anyabove = any(isabove),
    anybelow = any(isbelow),
    iabove = ifelse(anyabove, which(isabove & distance == min(distance[isabove])), NA),
    ibelow = ifelse(anybelow, which(isbelow & distance == min(distance[isbelow])), NA),
    isconvabove = ifelse(anyabove, isconv[iabove], NA),
    isconvbelow = ifelse(anybelow, isconv[ibelow], NA),
    ireached = ifelse(anyabove, ifelse(isconvabove, iabove, ifelse(anybelow, ifelse(isconvbelow, ibelow, NA), -Inf)), ifelse(anybelow, ifelse(isconvbelow, ibelow, Inf), NA)),
    xreached = ifelse(ireached %in% c(NA, -Inf, Inf), ireached, x[ireached]),
    conv = ifelse(ireached %in% c(NA, -Inf, Inf), NA, isconv[ireached]),
    inv = ifelse(ireached %in% c(NA, -Inf, Inf), NA, curv[ireached] > 0)
  ) %>%
  select(a, b, h, s, d, m, alpha, xreached, conv, inv) %>%
  mutate(branching = conv & inv) %>%
  add_column(anybp = smr1$anybp) %>%
  mutate(whatbp = ifelse(anybp, ifelse(branching, 2, 1), 0))
smr3$whatbp <- factor(smr3$whatbp)
levels(smr3$whatbp) <- c("No BP", "BP exists", "BP reachable")

p <- ggplot(smr3 %>% filter(branching), aes(x = h, y = s, fill = branching)) +
  geom_tile() +
  facet_grid(m ~ alpha, labeller = labeller(m = m_labs, alpha = alpha_labs)) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab("Habitat symmetry") +
  ylab("Ecological divergent selection") +
  scale_fill_manual(values = rgb(0.7, 0.7, 0.7))
p

ggsave("figures/branching_points_reached_large.png", p, width = 7, height = 7)

# Condensed version
smr4 <- smr3 %>% group_by(a, b, h, s, d, m) %>%
  summarize(alpha = max(alpha[branching]))
smr4$alpha[smr4$alpha == -Inf] <- NA

p <- ggplot(smr4 %>% filter(!is.na(alpha)), aes(x = h, y = s, fill = as.factor(alpha))) + 
  geom_tile() +
  facet_grid(. ~ m, labeller = labeller(m = m_labs)) +
  theme_bw() +
  scale_fill_manual(values = c(rgb(0.8, 0.8, 0.8), rgb(0.7, 0.7, 0.7), rgb(0.6, 0.6, 0.6))) +
  xlab("Habitat symmetry") +
  ylab("Ecological divergent selection") +
  labs(fill = "Sexual\nselection")
p

ggsave("figures/branching_points_reached.png", p, height = 3, width = 7)

# Combine both
p <- ggplot(smr3 %>% filter(whatbp != "No BP"), aes(x = h, y = s, fill = whatbp)) +
  geom_tile() +
  facet_grid(m ~ alpha, labeller = labeller(m = m_labs, alpha = alpha_labs)) +
  theme_bw() +
  xlab("Habitat symmetry") +
  ylab("Ecological divergent selection") +
  scale_fill_manual(values = c(rgb(0.7, 0.7, 0.7), rgb(0.6, 0.6, 0.6))) +
  labs(fill = "Branching point")
p

ggsave("figures/branching_points_combined.png", p, width = 8, height = 7)

################

# One patch model

rm(list = ls())
library(tidyverse)

data <- read.csv("data/adaptive_dynamics_patch1.csv")

data$isconv <- data$isconv == "True"
data[, c("x", "N", "curv")] <- sapply(data[, c("x", "N", "curv")], function(x) as.numeric(as.character(x)))
data[, c("a1", "a2", "b", "s", "d", "alpha")] <- sapply(data[, c("a1", "a2", "b", "s", "d", "alpha")], round, 3)

head(data)

smr <- data %>% group_by(a2, s) %>% summarize(anybp = any(isconv & curv > 0))
ggplot(smr, aes(x = a2, y = s, fill = anybp)) + geom_tile() 

# It works