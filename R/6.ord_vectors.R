#' Extract significant environmental vectors from vegan ordination object
#'
#' Performs an envfit analysis and returns environmental vectors with p-values,
#' filtered by significance or unfiltered.
#'
#' @param ord Vegan ordination object (e.g., rda, cca).
#' @param data Environmental variables data frame used for envfit.
#' @param p_thresh P-value threshold to determine significance (default 0.05).
#' @param filter Character; one of "significant" (default), "nonsignificant", or "all".
#'
#' @return Data frame of vectors with scores, p-values, and labels.
#'
#' @export
ord_vectors <- function(ord, data, p_thresh = 0.05,
                        filter = c("significant", "nonsignificant", "all")) {
  filter <- match.arg(filter)

  fit <- vegan::envfit(ord, data, permutations = 9999)
  vec <- as.data.frame(vegan::scores(fit, display = "vectors"))
  vec$pval <- fit$vectors$pvals
  vec$label <- rownames(vec)

  if (filter == "significant") {
    vec <- vec[vec$pval <= p_thresh, , drop = FALSE]
  } else if (filter == "nonsignificant") {
    vec <- vec[vec$pval > p_thresh, , drop = FALSE]
  } # else "all"

  vec
}
