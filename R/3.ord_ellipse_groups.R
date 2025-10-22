#' Compute ellipse coordinates for all groups in ordination scores
#'
#' Calculates ellipse coordinates for all groups in ordination scores,
#' supports classic and robust covariance estimation.
#'
#' @title Compute Ellipse Coordinates for All Groups
#' @description Calculates ellipse coordinates for all groups in ordination scores.
#' @param scores Data frame of ordination scores.
#' @param group_var Name of grouping column.
#' @param axis1 Name of first axis column.
#' @param axis2 Name of second axis column.
#' @param kind Type of ellipse: "se" or "sd".
#' @param conf Confidence level.
#' @param method Covariance method: "classic" or "robust".
#' @return Data frame of ellipse coordinates with group labels.
#'
#' @importFrom stats aggregate qchisq qt setNames
#' @importFrom grid unit
#'
#' @export
ord_ellipse_groups <- function(scores, group_var, axis1, axis2,
                               kind = "se", conf = 0.95, method = "classic") {
  groups <- unique(scores[[group_var]])
  ellipse_list <- lapply(groups, function(g) {
    grp <- subset(scores, scores[[group_var]] == g)
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
    coords <- ord_compute_ellipse(cov_mat,
      center = center,
      kind = kind, conf = conf, n = n
    )
    df <- as.data.frame(coords)
    names(df) <- c(axis1, axis2)
    df[[group_var]] <- g
    df
  })

  ellipses_df <- do.call(rbind, ellipse_list)
  rownames(ellipses_df) <- NULL
  ellipses_df
}
