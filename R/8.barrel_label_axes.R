#' Add Axis Titles with Method Name and Explained Variance
#'
#' This function creates properly formatted axis titles for ordination plots.
#' It uses the ordination method name (e.g., RDA, dbRDA, NMDS) and, when available,
#' the percentage of variance explained by the first two axes.
#'
#' The labels are retrieved using \code{\link{get_ord_axis_labels}} and
#' returned as a \code{ggplot2::labs()} object that can be added to a plot.
#'
#' @param ord A vegan ordination object, such as from \code{rda()}, \code{cca()},
#'   \code{capscale()}, or \code{metaMDS()}.
#'
#' @return A \code{ggplot2::labs()} object with \code{x} and \code{y} axis titles.
#'
#' @examples
#' library(vegan)
#' library(ggplot2)
#' data(dune)
#' data(dune.env)
#'
#' # Example with RDA
#' ord <- rda(dune)
#' scores_df <- as.data.frame(scores(ord, display = "sites"))
#' scores_df$Management <- dune.env$Management
#'
#' ggplot(scores_df, aes(x = PC1, y = PC2, color = Management)) +
#'   geom_point() +
#'   barrel_label_axes(ord) +
#'   theme_minimal() +
#'   theme(axis.title = element_text(face = "bold", size = 13))
#'
#' @seealso \code{\link{get_ord_axis_labels}}
#' @export
barrel_label_axes <- function(ord) {
  axis_labels <- get_ord_axis_labels(ord)$label
  ggplot2::labs(x = axis_labels[1], y = axis_labels[2])
}
