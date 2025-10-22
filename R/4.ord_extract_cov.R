#' Extract covariance matrix from ordination scores with optional robust estimation
#'
#' Computes the covariance matrix of ordination scores, optionally using robust estimation.
#'
#' @param scores Data frame of ordination scores.
#' @param axis1 Name of first axis column.
#' @param axis2 Name of second axis column.
#' @param weights Optional numeric vector of weights.
#' @param method Covariance method: "classic" or "robust".
#' @return 2x2 covariance matrix.
#' @export
ord_extract_cov <- function(scores, axis1, axis2, weights = NULL, method = c("classic", "robust")) {
  method <- match.arg(method)
  x <- scores[[axis1]]
  y <- scores[[axis2]]
  if (is.null(weights)) weights <- rep(1 / length(x), length(x))

  if (method == "robust") {
    use_robust <- length(x) >= 3
    if (use_robust) {
      cov_obj <- tryCatch(
        robustbase::covMcd(cbind(x, y), cor = FALSE),
        error = function(e) NULL
      )
      if (!is.null(cov_obj) &&
            is.matrix(cov_obj$cov) &&
            all(dim(cov_obj$cov) == c(2, 2)) &&
            !anyNA(cov_obj$cov)) {
        return(cov_obj$cov)
      }
    }
    warning("Robust covariance failed or skipped; falling back to classic.")
  }

  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  weights <- weights[ok]
  stats::cov.wt(cbind(x, y), wt = weights)$cov
}
