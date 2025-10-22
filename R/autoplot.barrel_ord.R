#' Autoplot method for barrel_ord objects
#'
#' Generates a ggplot of site scores from a vegan ordination object with optional group ellipses, centroids, and environmental arrows.
#' This method is designed to work with ordination objects prepared using \code{\link{barrel_prepare}}.
#'
#' @param object An object of class \code{"barrel_ord"} (a vegan ordination object with attached metadata).
#' @param ... Additional arguments:
#'   \describe{
#'     \item{group}{Character. Grouping variable in the metadata. (Required)}
#'     \item{data}{Optional species or environmental data for envfit.}
#'     \item{kind}{Type of ellipse: "se" or "sd". Default is "se".}
#'     \item{method}{Covariance method: "classic" or "robust". Default is "classic".}
#'     \item{conf}{Confidence level for ellipses. Default is 0.95.}
#'     \item{geom_type}{"polygon" or "path" for ellipse. Default is "polygon".}
#'     \item{show_arrows}{Logical; whether to show envfit vectors. Default is TRUE.}
#'     \item{show_centroids}{Logical. Default is FALSE.}
#'     \item{show_ellipses}{Logical. Default is TRUE.}
#'     \item{show_labels}{Logical. Label envfit arrows. Default is TRUE.}
#'     \item{p_thresh}{Numeric. Significance threshold for arrows. Default 0.05.}
#'     \item{alpha}{Numeric. Transparency of ellipses. Default is 0.5.}
#'   }
#' @return A ggplot2 object with ordination scores and optional graphical layers.
#'
#' @details
#' If \code{geom_type = "polygon"}, group colors are mapped to \code{fill} for ellipses. If \code{geom_type = "path"}, group colors are mapped to \code{color}.
#' The grouping variable must be present in the metadata provided to \code{\link{barrel_prepare}}.
#'
#' @seealso \code{\link{barrel_prepare}}, \code{\link{stat_barrel}}, \code{\link[vegan]{rda}}, \code{\link[vegan]{metaMDS}}
#'
#' @examples
#' library(vegan)
#' data(dune)
#' data(dune.env)
#' ord <- rda(dune, scale = TRUE)
#' ord <- barrel_prepare(ord, dune.env)
#' ggplot2::autoplot(ord,
#'   group = "Management", data = dune,
#'   method = "robust", kind = "sd", geom_type = "polygon", show_arrows = TRUE
#' )
#'
#' @importFrom ggplot2 autoplot
#' @importFrom ggplot2 ggplot aes_string geom_point labs theme_minimal
#' @importFrom rlang %||% .data
#' @method autoplot barrel_ord
#' @export
autoplot.barrel_ord <- function(object, ...) {
  args <- list(...)
  get_arg <- function(name, default) if (!is.null(args[[name]])) args[[name]] else default

  group <- get_arg("group", NULL)
  data <- get_arg("data", NULL)
  kind <- get_arg("kind", "se")
  method <- get_arg("method", "classic")
  conf <- get_arg("conf", 0.95)
  geom_type <- get_arg("geom_type", "polygon")
  show_arrows <- get_arg("show_arrows", TRUE)
  show_centroids <- get_arg("show_centroids", FALSE)
  show_ellipses <- get_arg("show_ellipses", TRUE)
  show_labels <- get_arg("show_labels", TRUE)
  p_thresh <- get_arg("p_thresh", 0.05)
  alpha <- get_arg("alpha", 0.5)

  ord <- object
  metadata <- attr(ord, "barrel_meta")

  if (is.null(metadata) || is.null(group) || !(group %in% names(metadata))) {
    stop("Grouping variable not found in ordination metadata.")
  }

  # Obtener puntuaciones y etiquetas
  scores_mat <- vegan::scores(ord, display = "sites")
  axis_labels <- get_ord_axis_labels(ord)

  # Convertir a data.frame y renombrar columnas para el mapeo
  scores_df <- as.data.frame(scores_mat)
  colnames(scores_df)[1:2] <- c("axis1", "axis2")
  scores_df[[group]] <- metadata[[group]]

  p <- ggplot2::ggplot(scores_df, ggplot2::aes(
    x = .data[["axis1"]],
    y = .data[["axis2"]],
    color = .data[[group]],
    group = .data[[group]],
    fill = .data[[group]],
    shape = .data[[group]]
  ))

  if (show_ellipses) {
    p <- p + stat_barrel(
      method = method, kind = kind, conf = conf,
      geom_type = geom_type, alpha = alpha, color = "black"
    )
  }
  if (show_centroids) {
    p <- p + stat_barrel_centroid(method = method, shape = 3)
  }
  if (show_arrows && !is.null(data)) {
    p <- p + stat_barrel_arrows(
      ord = ord, matrix = data,
      labels = show_labels,
      show.significant = TRUE, p_thresh = p_thresh
    )
  }

  p + ggplot2::geom_point() +
    stat_barrel_annotate(ord = ord) +
    ggplot2::labs(x = axis_labels$label[1], y = axis_labels$label[2]) +
    ggplot2::theme_minimal()
}
