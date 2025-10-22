library(testthat)
library(vdiffr)

test_that("autoplot.barrel_ord generates expected output", {
  library(vegan)
  data(dune)
  data(dune.env)

  # Preparar ordination y metadata
  ord <- rda(dune, scale = TRUE)
  ord <- barrel_prepare(ord, dune.env)

  # Generar gráfico con autoplot()
  p <- ggplot2::autoplot(
    ord,
    group = "Management",
    data = dune,
    method = "classic",
    kind = "sd",
    geom_type = "polygon",
    show_arrows = FALSE,
    show_centroids = TRUE,
    show_ellipses = TRUE,
    alpha = 0.5
  )

  # Comparar visualmente con snapshot
  vdiffr::expect_doppelganger("autoplot barrel_ord (rda, sd, polygon)", p)
})
