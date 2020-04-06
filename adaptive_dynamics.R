rm(list = ls())

# Here we make figures to show the outcome of the adaptive dynamics across parameter
# space, from data generated in Mathematica.

#### Accessory functions ####

mathematica2r <- function(data, numerics, digits = 4) {
  
  library(tidyverse)
  
  data %>% 
    mutate_if(~ any(.x == "True"), ~ .x == "True") %>%
    mutate_at(numerics, ~ round(as.numeric(as.character(.x)), digits))
  
}

predict_adaptive_dynamics <- function(data, xstart) {
  
  library(tidyverse)
  
  data %>% mutate(
    isbp = isconv & curv > 0,
    isabove = x > xstart,
    isbelow = x <= xstart,
    distance = abs(x - xstart)
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
      
    )
}

make_facet_labels <- function(data, varname) {
  
  labs <- unique(unlist(data[, varname]))
  labnames <- as.character(labs)
  labs <- paste(varname, labs, sep = " = ")
  names(labs) <- labnames
  return (labs)
  
}

plot_divergence_map <- function(data, xstart, keep_legend = TRUE) {
  
  library(tidyverse)
  library(cowplot)
  
  # Summarize the adaptive dynamics given a starting point
  smr <- data %>% 
    
    # Evaluate the attained equilibrium for each parameter set in each patch
    group_by(a, h, b, s, d, alpha, patch) %>% 
    predict_adaptive_dynamics(xstart = xstart) %>%
    select(a, h, b, s, d, alpha, patch, anybp, xreached, conv, inv) %>%
    mutate(branching = conv & inv) %>% 
    
    # Calculate the difference between patches
    group_by(a, h, b, s, d, alpha) %>%
    summarize(
      diff = dist(xreached), # distance in equilibria reached
      bothbranch = all(branching) # does branching occur in both patches?
    ) %>%
    filter(!is.na(diff))
  
  # Plot the divergence in evolutionary outcome between the patches
  p <- ggplot(smr, aes(x = h, y = s, fill = diff)) + 
    geom_tile() +
    scale_fill_gradient(low = "black", high = "lightblue") +
    theme_bw() +
    ylab("Ecological divergent selection") +
    xlab("Habitat symmetry") +
    labs(fill = "Allopatric\ndivergence") +
    xlim(c(0, 1)) +
    
    # Delineate where within-patch branching occurs
    geom_polygon(data = smr %>% 
                   filter(bothbranch) %>%
                   group_by(s) %>%
                   summarize(h = min(h)) %>%
                   add_row(s = max(smr$s[smr$bothbranch]), h = max(smr$h), .before = TRUE) %>%
                   add_row(s = min(smr$s[smr$bothbranch]), h = max(smr$h), .after = TRUE), 
                 aes(x = h, y = s, fill = NULL), alpha = 0.3, fill = "yellow", color = "black", lty = 3) +
    theme(legend.key.height = unit(7, "mm"), legend.margin = margin(0, 0, 0, 0))
  
  # Manipulate legends
  leg1 <- get_legend(p)
  leg2 <- get_legend(
    ggplot(data.frame(x = 1, y = 1, log = "Sympatric\nbranching"), aes(x = x, y = y, alpha = log)) + 
      geom_tile(fill = "yellow") + 
      scale_alpha_manual(values = 0.3) + 
      labs(alpha = NULL) + 
      theme(legend.margin = margin(0, 0, 30, 0))
  )
  legs <- plot_grid(leg1, leg2, nrow = 2, rel_heights = c(2, 1))
  
  p <- p + theme(legend.position = "none")
  
  if (!keep_legend) return (p)
  
  plot_grid(p, legs, ncol = 2, rel_widths = c(2, 1))  
  
}


#### Analysis ####

library(tidyverse)

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

# One patch model

library(tidyverse)
library(cowplot)

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

