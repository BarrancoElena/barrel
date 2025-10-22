#' Compute ellipse coordinates for a specific group in ordination scores
#'
#' Calculates ellipse coordinates for one group from ordination scores,
#' supporting classical or robust covariance estimation.
#'
#' @title Compute Ellipse Coordinates for a Single Group
#' @description Calculates ellipse coordinates for one group in ordination scores with robust or classic covariance.
#' @param scores Data frame of ordination scores.
#' @param group_var Name of grouping column.
#' @param group_name Name of the group to calculate ellipse.
#' @param axis1 Name of first axis column.
#' @param axis2 Name of second axis column.
#' @param kind Type of ellipse: "se" or "sd".
#' @param conf Confidence level.
#' @param method Covariance method: "classic" or "robust".
#' @return Data frame of ellipse coordinates with group label.
#' @export

#'
#' @return A data frame with ellipse coordinates and group label.
#'
#' @importFrom stats aggregate qchisq qt setNames
#' @importFrom grid unit
#'
#' @examples
#' library(vegan)
#' data(dune)
#' data(dune.env)
#' pca <- rda(dune, scale = TRUE)
#' scores <- as.data.frame(scores(pca, display = "sites"))
#' scores$Group <- dune.env$Management
#' ell <- ord_ellipse_group(scores, "Group", "BF", "PC1", "PC2", kind = "se", method = "classic")
#' plot(ell$PC1, ell$PC2, type = "l")
#'
#' @export
ord_ellipse_group <- function(scores, group_var, group_name, axis1, axis2,
                              kind = "se", conf = 0.95, method = "classic") {
  grp <- subset(scores, scores[[group_var]] == group_name)
  x <- grp[[axis1]]
  y <- grp[[axis2]]
  n <- length(x)

  cov_mat <- if (method == "robust" && n >= 3) {
    tryCatch(robustbase::covMcd(cbind(x, y))$cov,
      error = function(e) stats::cov(cbind(x, y))
    )
  } else {
    stats::cov(cbind(x, y))
  }

  center <- c(mean(x), mean(y))
  ellipse_coords <- ord_compute_ellipse(cov_mat,
    center = center,
    kind = kind, conf = conf, n = n
  )
  df <- as.data.frame(ellipse_coords)
  names(df) <- c(axis1, axis2)
  df[[group_var]] <- group_name
  df
}
