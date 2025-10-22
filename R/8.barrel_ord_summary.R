#' Summary Statistics for Ordination Objects
#'
#' Computes adjusted R-squared and percentage of variance explained for constrained ordination methods
#' (e.g., \code{rda}, \code{cca}, \code{capscale}, \code{dbrda}), or returns the stress value for
#' non-metric multidimensional scaling (\code{metaMDS}).
#'
#' @param ord An ordination object of class \code{rda}, \code{cca}, \code{capscale}, \code{dbrda}, or \code{metaMDS}.
#'
#' @return A named list with components:
#' \describe{
#'   \item{method}{Class of the ordination object as a character string.}
#'   \item{R2_adj}{Adjusted R-squared (percentage) for constrained ordinations; \code{NA} for NMDS.}
#'   \item{axis_var}{Named numeric vector of variance explained by each axis (percentage), if available.}
#'   \item{stress}{Stress value for NMDS objects.}
#'   \item{message}{Message explaining output, especially for NMDS or missing R2.}
#' }
#'
#' @examples
#' library(vegan)
#' data(dune)
#' data(dune.env)
#'
#' # PCA (unconstrained ordination)
#' pca <- rda(dune)
#' barrel_ord_summary(pca)
#'
#' # RDA (constrained ordination)
#' rda_mod <- rda(dune ~ A1 + Moisture, data = dune.env)
#' barrel_ord_summary(rda_mod)
#'
#' # NMDS
#' nmds <- metaMDS(dune, k = 2)
#' barrel_ord_summary(nmds)
#'
#' # dbRDA
#' dbrda_mod <- dbrda(dune ~ A1 + Management, data = dune.env, distance = "bray")
#' barrel_ord_summary(dbrda_mod)
#'
#' @importFrom vegan RsquareAdj eigenvals
#' @export
barrel_ord_summary <- function(ord) {
  if (inherits(ord, "metaMDS")) {
    return(list(
      method = "NMDS",
      stress = ord$stress,
      message = "R\u00B2 and axis variance not available for NMDS"
    ))
  }

  r2_vals <- tryCatch(
    vegan::RsquareAdj(ord),
    error = function(e) NULL
  )

  r2 <- if (!is.null(r2_vals) && length(r2_vals$r.squared) == 1) r2_vals$r.squared else NA_real_
  r2_adj <- if (!is.null(r2_vals) && length(r2_vals$adj.r.squared) == 1) r2_vals$adj.r.squared else NA_real_

  axis_var <- tryCatch(
    {
      eigs <- vegan::eigenvals(ord)
      eigs / sum(eigs) * 100
    },
    error = function(e) rep(NA_real_, 2)
  )

  axis_var <- setNames(axis_var, paste0("Axis", seq_along(axis_var)))

  list(
    method = class(ord)[1],
    R2 = r2,
    R2_adj = r2_adj,
    axis_var = axis_var,
    message = if (is.na(r2_adj)) "No R\u00B2 available (possibly PCA without constraints)." else NULL
  )
}
