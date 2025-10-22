#' Compute coordinates for an ordination ellipse
#'
#' Calculates ellipse coordinates based on a covariance matrix and center.
#' Supports confidence ellipses (standard error, "se") or dispersion ellipses ("sd").
#'
#' @param cov Numeric matrix (2x2) covariance matrix.
#' @param center Numeric vector of length 2 specifying the ellipse center coordinates.
#' @param kind Character string specifying the type of ellipse: either
#'   \code{"se"} for standard error/confidence ellipse or \code{"sd"} for standard deviation dispersion ellipse.
#' @param conf Numeric confidence level or coverage (default 0.95).
#' @param npoints Integer number of points to generate along ellipse perimeter (default 100).
#' @param n Numeric sample size; **required** if \code{kind = "se"}.
#'
#' @return A numeric matrix with two columns representing x and y coordinates of the ellipse.
#'
#' @examples
#' cov_mat <- matrix(c(1, 0, 0, 1), 2, 2)
#' center <- c(0, 0)
#' ell_coords <- ord_compute_ellipse(cov_mat, center, kind = "se", conf = 0.95, n = 10)
#' plot(ell_coords, type = "l")
#'
#' @importFrom stats qt qchisq
#' @export
ord_compute_ellipse <- function(cov, center = c(0, 0), kind = "se", conf = 0.95,
                                npoints = 100, n = NULL) {
  if (kind == "se") {
    if (is.null(n)) stop("Sample size 'n' must be provided when kind = 'se'")
    cov <- cov / n
    scale <- stats::qt(conf + (1 - conf) / 2, df = n - 1)
  } else if (kind == "sd") {
    scale <- sqrt(stats::qchisq(conf, df = 2))
  } else {
    stop("Argument 'kind' must be 'se' or 'sd'")
  }
  theta <- seq(0, 2 * pi, length.out = npoints + 1)
  circle <- cbind(cos(theta), sin(theta))
  ellipse <- t(center + scale * t(circle %*% chol(cov)))
  ellipse
}
