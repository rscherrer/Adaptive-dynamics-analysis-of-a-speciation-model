#' Plot divergence map
#'
#' Heatmap of trait divergence between the two patches in the absence of dispersal (one-patch model). A transparent extra layer indicates whether branching occurs within both patch (relevant only if both patches are mirror images of each other).
#'
#' @param data A dataset of singularities for the one-patch model
#' @param xstart The initial trait value
#' 
#' @export

plot_divergence_map <- function(data, xstart) {

  library(tidyverse)

  # Summarize the adaptive dynamics given a starting point
  smr <- data %>%

    # Evaluate the attained equilibrium for each parameter set in each patch
    group_by(a, h, b, s, d, alpha, patch) %>%
    predict_adaptive_dynamics(xstart = xstart) %>%
    select(a, h, b, s, d, alpha, patch, anybp, xreached, convergent, invasible) %>%
    mutate(branching = convergent & invasible) %>%

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
    xlim(c(0, 1)) +
    theme(legend.key.height = unit(7, "mm"), legend.margin = margin(0, 0, 0, 0))
  
  # Delineate where within-patch branching occurs, for each value of the sexual selection coefficient
  p <- p + geom_tile(data = smr %>%
                  filter(bothbranch) %>%
                  group_by(h, s) %>%
                  summarize(alpha = max(alpha))
                , aes(x = h, y = s, alpha = factor(alpha), fill = NULL), fill = "yellow") +
    scale_alpha_manual(values = c(0.3, 0.5, 0.7)) +
    labs(alpha = "Sexual selection for which\nsympatric branching is observed", fill = "Allopatric divergence") +
    theme(legend.position = "top")
  
  return (p)

}
