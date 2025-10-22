library(testthat)
library(vdiffr)
test_that("stat_barrel layers combine correctly in a full ordination plot", {
  library(ggplot2)
  library(robustbase)
  library(vegan)
  data(dune)
  data(dune.env)
  set.seed(123)
  # Preparar objeto
  ord <- rda(dune, scale = TRUE)
  ord <- barrel_prepare(ord, dune.env)
  scores <- as.data.frame(vegan::scores(ord, display = "sites"))
  scores$Management <- dune.env$Management

  # Gráfico con capas combinadas
  p <- ggplot2::ggplot(scores, ggplot2::aes(x = PC1, y = PC2, group = Management, fill = Management)) +
    stat_barrel(
      method = "classic", kind = "sd", conf = 0.95,
      geom_type = "polygon", alpha = 0.4, color = "black"
    ) +
    stat_barrel_centroid(method = "classic", shape = 3) +
    stat_barrel_arrows(
      ord = ord, matrix = dune,
      labels = FALSE, labels.color = "blue",
      arrow.color = "darkred", arrow.linetype = "solid",
      labels.fontface = "bold", show.significant = TRUE
    ) +
    stat_barrel_annotate(ord = ord, xpad = 0.05, ypad = 0.05) +
    ggplot2::geom_point(ggplot2::aes(color = Management)) +
    ggplot2::theme_minimal()

  # Comparar visualmente
  vdiffr::expect_doppelganger("barrel full ordination layers", p)
})
