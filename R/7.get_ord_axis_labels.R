#' Internal function to get axis labels for vegan ordination objects
#'
#' This function infers the ordination method used and returns proper axis labels
#' including percentage of explained variance when applicable.
#'
#' @param ord A vegan ordination object.
#' @return A list with one element `label`, a character vector for axis labels.
#' #' @examples
#' library(vegan)
#' data(dune)
#' data(dune.env)
#' ord_pca <- rda(dune)
#' get_ord_axis_labels(ord_pca)
#'
#' @export
get_ord_axis_labels <- function(ord) {
  scores_mat <- vegan::scores(ord, display = "sites")
  n_axes <- ncol(scores_mat)

  make_labels <- function(tag, prop = NULL, n_axes = 2L) {
    if (is.null(prop)) {
      return(paste0(tag, seq_len(n_axes)))
    } else {
      k <- min(length(prop), n_axes)
      labs <- paste0(tag, seq_len(k), " (", round(100 * prop[seq_len(k)], 1), "%)")
      if (k < n_axes) {
        labs <- c(labs, paste0(tag, seq((k + 1L), n_axes)))
      }
      return(labs)
    }
  }
  if (inherits(ord, "metaMDS"))  return(list(label = make_labels("NMDS", n_axes = n_axes)))
  if (inherits(ord, "decorana")) return(list(label = make_labels("DCA",  n_axes = n_axes)))
  if (inherits(ord, "capscale")) {
    eig_con <- tryCatch(vegan::eigenvals(ord, model = "constrained"),
                        error = function(e) numeric())
    eig_con <- eig_con[is.finite(eig_con) & eig_con > 0]

    if (length(eig_con) > 0) {
      prop <- eig_con / sum(eig_con, na.rm = TRUE)
      return(list(label = make_labels("dbRDA", prop = prop, n_axes = n_axes)))
    } else {
      eig_un <- tryCatch(vegan::eigenvals(ord, model = "unconstrained"),
                         error = function(e) numeric())
      eig_un <- eig_un[is.finite(eig_un) & eig_un > 0]
      prop <- if (length(eig_un) > 0) eig_un / sum(eig_un, na.rm = TRUE) else NULL
      return(list(label = make_labels("CAP", prop = prop, n_axes = n_axes)))
    }
  }
  if (inherits(ord, "rda")) {
    if (!is.null(ord$CCA) && length(ord$CCA$eig) > 0) {
      eig <- ord$CCA$eig
      prop <- eig / sum(eig, na.rm = TRUE)
      return(list(label = make_labels("RDA", prop = prop, n_axes = n_axes)))
    } else {
      eig <- ord$CA$eig
      prop <- eig / sum(eig, na.rm = TRUE)
      return(list(label = make_labels("PC", prop = prop, n_axes = n_axes)))
    }
  }
  if (inherits(ord, "cca")) {
    if (!is.null(ord$CCA) && length(ord$CCA$eig) > 0) {
      eig <- ord$CCA$eig
      prop <- eig / sum(eig, na.rm = TRUE)
      return(list(label = make_labels("CCA", prop = prop, n_axes = n_axes)))
    } else {
      eig <- ord$CA$eig
      prop <- eig / sum(eig, na.rm = TRUE)
      return(list(label = make_labels("CA", prop = prop, n_axes = n_axes)))
    }
  }
  return(list(label = make_labels("Axis", n_axes = n_axes)))
}
