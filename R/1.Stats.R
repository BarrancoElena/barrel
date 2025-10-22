#' Comprehensive Ordination Visualization Layer
#'
#' This set of ggplot2 stats draws ellipses around groups, centroids, environmental arrows,
#' and adds adjusted R^2 or stress annotation for ordination objects from vegan.
#'
#' @title Comprehensive Ordination Visualization Layer
#' @description Combines ordination ellipses, centroids, environmental arrows, and annotations into a ggplot2 layer.
#'
#' @param mapping Set of aesthetic mappings created by \code{aes()}.
#' @param data Data frame used for plotting.
#' @param ord Ordination object from vegan, e.g. \code{rda}, \code{cca}, \code{dbrda}, or \code{metaMDS}.
#' @param geom Character; geometric object to use for ellipses, either \code{"path"} or \code{"polygon"}.
#' @param geom_type Character; geometric object to use for ellipses, either \code{"path"} or \code{"polygon"}.
#' @param position Position adjustment for ggplot2 layers (default \code{"identity"}).
#' @param kind Character; type of ellipse: \code{"se"} (standard error), \code{"sd"} (dispersion), or \code{"ci"} (confidence interval).
#' @param conf Numeric; confidence level for ellipses (default 0.95).
#' @param method Character; covariance estimation method: \code{"classic"} or \code{"robust"}.
#' @param shape Integer; shape code for centroid points (default 3).
#' @param matrix Optional data frame of environmental variables for arrows.
#' @param labels Logical; whether to show labels on environmental arrows (default \code{TRUE}).
#' @param labels.color Color of arrow labels (default \code{"black"}).
#' @param labels.size Numeric size of arrow labels (default 3).
#' @param labels.fontface Font face of arrow labels (default \code{"plain"}).
#' @param labels.max.overlaps Maximum number  of labels in the same space (default \code{"Inf"}).
#' @param arrow A grid arrow object controlling arrow appearance (default created by \code{grid::arrow()}).
#' @param arrow.color Color of arrows (default \code{"grey10"}).
#' @param arrow.linetype Line type of arrows (default \code{"solid"}).
#' @param arrow.size Numeric line width of arrows (default 0.3).
#' @param show.significant Logical; whether to show only significant arrows (default \code{FALSE}).
#' @param p_thresh Numeric; p-value threshold for significance filtering (default 0.05).
#' @param xpad Numeric; horizontal padding for annotation text relative to x-axis range (default 0.05).
#' @param ypad Numeric; vertical padding for annotation text relative to y-axis range (default 0.05).
#' @param hjust Numeric; horizontal justification of annotation text (default 0).
#' @param vjust Numeric; vertical justification of annotation text (default 1).
#' @param show.legend Logical; whether to show legends (default \code{NA}).
#' @param inherit.aes Logical; whether to inherit aesthetics (default \code{TRUE}).
#' @param ... Additional parameters passed to underlying geoms or stats.
#'
#' @return A ggplot2 layer combining ellipses, centroids, arrows, and annotations.
#'
#' @examples
#' library(ggplot2)
#' library(vegan)
#' library(robustbase)
#' data(dune)
#' data(dune.env)
#'
#' pca <- vegan::rda(dune, scale = TRUE)
#' scores <- as.data.frame(vegan::scores(pca, display = "sites"))
#' scores$Management <- dune.env$Management
#'
#' ggplot(scores, aes(PC1, PC2, group = Management, fill = Management)) +
#'   stat_barrel(
#'     method = "classic", kind = "se", conf = 0.95, geom_type = "polygon",
#'     alpha = 0.4, color = "black"
#'   ) +
#'   stat_barrel_centroid(method = "classic", shape = 3) +
#'   stat_barrel_arrows(
#'     ord = pca, matrix = dune,
#'     labels = TRUE, labels.color = "blue",
#'     arrow.color = "darkred", arrow.linetype = "solid",
#'     labels.fontface = "bold", show.significant = TRUE,
#'     labels.overlaps = Inf,
#'   ) +
#'   stat_barrel_annotate(ord = pca, xpad = 0.05, ypad = 0.05) +
#'   geom_point(aes(color = Management)) +
#'   theme_minimal()
#'
#' @name barrel_stats
#' @export
stat_barrel <- function(mapping = NULL, data = NULL,
                        geom = "path", position = "identity",
                        kind = "se", conf = 0.95,
                        method = c("robust", "classic"),
                        show.legend = NA,
                        inherit.aes = TRUE,
                        geom_type = c("path", "polygon"),
                        ...) {
  method <- match.arg(method)
  geom_type <- match.arg(geom_type)
  geom_fn <- if (geom_type == "polygon") "polygon" else "path"

  ggplot2::layer(
    stat = StatBarrel, data = data, mapping = mapping,
    geom = geom_fn, position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(kind = kind, conf = conf, method = method, ...)
  )
}

#' @rdname barrel_stats
#' @export
stat_barrel_centroid <- function(mapping = NULL, data = NULL,
                                 geom = "point", position = "identity",
                                 method = c("classic", "robust"),
                                 show.legend = NA,
                                 inherit.aes = TRUE,
                                 shape = 3,
                                 ...) {
  method <- match.arg(method)
  ggplot2::layer(
    stat = StatBarrelCentroid, data = data, mapping = mapping,
    geom = geom, position = position, show.legend = show.legend,
    inherit.aes = inherit.aes, params = list(method = method, shape = shape, ...)
  )
}

#' @rdname barrel_stats
#' @export
stat_barrel_arrows <- function(mapping = NULL, data = NULL,
                               ord, matrix,
                               geom = "segment", position = "identity",
                               labels = TRUE,
                               labels.color = "black",
                               labels.size = 3,
                               labels.fontface = "plain",
                               show.significant = FALSE,
                               p_thresh = 0.05,
                               arrow = grid::arrow(length = grid::unit(0.25, "cm")),
                               arrow.color = "grey10",
                               arrow.linetype = "solid",
                               arrow.size = 0.3,
                               labels.max.overlaps = Inf,
                               show.legend = NA,
                               inherit.aes = TRUE,
                               ...) {
  vectors_raw <- compute_envfit_vectors(ord, matrix, p_thresh, show.significant)
  vectors <- data.frame(
    x = 0,
    y = 0,
    xend = vectors_raw[, 1],
    yend = vectors_raw[, 2],
    label = vectors_raw$label
  )

  arrow_layer <- ggplot2::geom_segment(
    data = vectors,
    mapping = ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
    arrow = arrow,
    color = arrow.color,
    linetype = arrow.linetype,
    linewidth = arrow.size,
    inherit.aes = FALSE,
    ...
  )

  if (labels) {
    label_layer <- ggrepel::geom_text_repel(
      data = vectors,
      mapping = ggplot2::aes(x = xend, y = yend, label = label),
      color = labels.color,
      size = labels.size,
      fontface = labels.fontface,
      max.overlaps = labels.max.overlaps,
      inherit.aes = FALSE
    )
    return(list(arrow_layer, label_layer))
  }

  return(arrow_layer)
}

utils::globalVariables(c("x", "y", "xend", "yend", "label"))

#' @rdname barrel_stats
#' @export
compute_envfit_vectors <- function(ord, matrix, p_thresh = 0.05, show.significant = FALSE) {
  fit <- vegan::envfit(ord, matrix, permutations = 9999)
  vec <- as.data.frame(vegan::scores(fit, display = "vectors"))
  vec$pval <- fit$vectors$pvals
  vec$label <- rownames(vec)
  if (show.significant) {
    vec <- vec[vec$pval <= p_thresh, , drop = FALSE]
  }
  vec
}

#' @rdname barrel_stats
#' @export
stat_barrel_annotate <- function(mapping = NULL, data = NULL, ord,
                                 position = "identity",
                                 show.legend = FALSE, inherit.aes = TRUE,
                                 xpad = 0.05, ypad = 0.05,
                                 hjust = 0, vjust = 1,
                                 ...) {
  ggplot2::layer(
    stat = StatBarrelAnnotate, data = data, mapping = mapping,
    geom = "text", position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      ord = ord,
      xpad = xpad, ypad = ypad,
      hjust = hjust, vjust = vjust,
      ...
    )
  )
}
#' @rdname barrel_stats
#' @format NULL
#' @usage NULL
#' @export
StatBarrel <- ggplot2::ggproto("StatBarrel", ggplot2::Stat,
  required_aes = c("x", "y", "group"),
  compute_group = function(data, scales, kind = "se", conf = 0.95, method = "robust") {
    n <- nrow(data)

    if (method == "classic" || n < 3) {
      center <- c(mean(data$x), mean(data$y))
      cov_mat <- stats::cov(cbind(data$x, data$y))
    } else {
      covrob <- tryCatch(
        robustbase::covMcd(cbind(data$x, data$y)),
        error = function(e) {
          message("covMcd failed, falling back to classic method.")
          list(
            center = c(mean(data$x), mean(data$y)),
            cov = stats::cov(cbind(data$x, data$y))
          )
        }
      )
      center <- covrob$center
      cov_mat <- covrob$cov
    }

    if (kind == "se") {
      cov_mat <- cov_mat / n
      scale <- stats::qt(conf + (1 - conf) / 2, df = n - 1)
    } else if (kind == "ci") {
      scale <- stats::qt(conf + (1 - conf) / 2, df = n - 1)
    } else if (kind == "sd") {
      scale <- sqrt(stats::qchisq(conf, df = 2))
    } else {
      stop("kind must be one of 'se', 'ci', or 'sd'")
    }

    theta <- seq(0, 2 * pi, length.out = 101)
    circle <- cbind(cos(theta), sin(theta))
    coords <- t(center + scale * t(circle %*% chol(cov_mat)))
    data.frame(x = coords[, 1], y = coords[, 2])
  }
)

#' @rdname barrel_stats
#' @format NULL
#' @usage NULL
#' @export
StatBarrelCentroid <- ggplot2::ggproto("StatBarrelCentroid", ggplot2::Stat,
  required_aes = c("x", "y", "group"),
  compute_group = function(data, scales, method = "classic") {
    if (method == "classic") {
      center_x <- mean(data$x)
      center_y <- mean(data$y)
    } else if (method == "robust") {
      center <- robustbase::covMcd(data[, c("x", "y")])$center
      center_x <- center[1]
      center_y <- center[2]
    } else {
      stop("Unknown method. Use 'classic' or 'robust'.")
    }
    data.frame(x = center_x, y = center_y)
  }
)

#' @rdname barrel_stats
#' @format NULL
#' @usage NULL
#' @export
StatBarrelAnnotate <- ggplot2::ggproto("StatBarrelAnnotate", ggplot2::Stat,
  required_aes = c("x", "y"),
  compute_panel = function(data, scales, ord, xpad, ypad, hjust, vjust) {
    summ <- barrel_ord_summary(ord)

    if (inherits(ord, "metaMDS")) {
      if (!is.null(summ$stress) && !is.na(summ$stress)) {
        label_text <- paste0("Stress: ", round(summ$stress, 3))
      } else {
        return(data.frame())
      }
    } else {
      r2 <- summ$R2
      r2_adj <- summ$R2_adj

      if (!is.na(r2_adj) && r2_adj >= 0) {
        label_text <- paste0("Adj R\u00B2: ", sprintf("%.1f%%", 100 * r2_adj))
      } else if (!is.na(r2)) {
        message("Adjusted R\u00B2 is negative or unavailable; showing raw R\u00B2 instead.")
        label_text <- paste0("R\u00B2: ", sprintf("%.1f%%", 100 * r2))
      } else {
        message("R\u00B2 and Adjusted R\u00B2 are not available; no annotation will be drawn.")
        return(data.frame())
      }
    }

    xmin <- min(data$x, na.rm = TRUE)
    ymin <- min(data$y, na.rm = TRUE)
    xrange <- max(data$x, na.rm = TRUE) - xmin
    yrange <- max(data$y, na.rm = TRUE) - ymin

    xpos <- xmin + xpad * xrange
    ypos <- ymin - ypad * yrange

    data.frame(
      x = xpos, y = ypos, label = label_text,
      hjust = hjust, vjust = vjust
    )
  }
)
