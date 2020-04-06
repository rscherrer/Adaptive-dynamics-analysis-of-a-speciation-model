#' Predict the adaptive dynamics
#'
#' Makes a summary dataset containing the attained singularity given a starting trait value and a map of singular points throughout parameter space.
#'
#' @param data A dataset of singularities
#' @param xstart The initial trait value
#'
#' @export

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
