#' Prepare ordination object for barrel plotting
#'
#' Attaches sample metadata (e.g., grouping variable) to a vegan ordination object and assigns class 'barrel_ord'.
#'
#' @param ord A vegan ordination object (e.g., from \code{rda}, \code{cca}, \code{metaMDS}).
#' @param metadata A data frame of sample metadata. Must have same number of rows as ordination site scores.
#'
#' @return The ordination object with metadata attached and class \code{"barrel_ord"}.
#' @export
#'
#' @examples
#' library(vegan)
#' data(dune)
#' data(dune.env)
#' ord <- rda(dune, scale = TRUE)
#' ord_prepared <- barrel_prepare(ord, dune.env)
#' class(ord_prepared)
barrel_prepare <- function(ord, metadata) {
  if (!inherits(ord, c("rda", "cca", "metaMDS", "capscale", "dbrda"))) {
    stop("Unsupported ordination object.")
  }
  scores_sites <- vegan::scores(ord, display = "sites")
  if (!is.data.frame(scores_sites)) scores_sites <- as.data.frame(scores_sites)

  if (nrow(scores_sites) != nrow(metadata)) {
    stop(paste0(
      "Rows in ordination scores (", nrow(scores_sites),
      ") and metadata (", nrow(metadata), ") do not match."
    ))
  }

  attr(ord, "barrel_meta") <- metadata
  class(ord) <- unique(c("barrel_ord", class(ord))) # asegura que no se repita
  message("barrel_prepare() successful: metadata attached and class 'barrel_ord' assigned.")
  ord
}
