#' Extract centroids of groups from ordination scores with classic or robust method
#'
#' Computes the centroid coordinates for each group using either the classical mean
#' or robust minimum covariance determinant.
#'
#' @title Extract Group Centroids from Ordination Scores
#' @description Computes centroids per group using classic mean or robust methods.
#' @param scores Data frame of ordination scores.
#' @param group_var Name of grouping column.
#' @param axis1 Name of first axis column.
#' @param axis2 Name of second axis column.
#' @param method Centroid method: "classic" or "robust".
#' @return Data frame with group centroids.
#' @export
ord_extract_centroids <- function(scores, group_var, axis1, axis2, method = c("classic", "robust")) {
  method <- match.arg(method)
  if (method == "classic") {
    aggregate(scores[, c(axis1, axis2)], by = list(scores[[group_var]]), FUN = mean, na.rm = TRUE)
  } else if (method == "robust") {
    groups <- unique(scores[[group_var]])
    res <- lapply(groups, function(g) {
      grp <- subset(scores, scores[[group_var]] == g)
      center <- tryCatch(robustbase::covMcd(grp[, c(axis1, axis2)])$center,
        error = function(e) colMeans(grp[, c(axis1, axis2)], na.rm = TRUE)
      )
      data.frame(group = g, t(center))
    })
    do.call(rbind, res)
  }
}
