#' Plot divergence map
#'
#' Heatmap of trait divergence between the two patches in the absence of dispersal (one-patch model). A transparent extra layer indicates whether branching occurs within both patch (relevant only if both patches are mirror images of each other).
#'
#' @param data A dataset of singularities for the one-patch model
#' @param xstart The initial trait value
#' @param keep_legend Whether to return the legend

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
