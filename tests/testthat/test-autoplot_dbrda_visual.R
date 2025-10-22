# This test may fail intermittently due to minor label shifts caused by ggrepel::geom_text_repel().
# Such changes are expected and do not indicate a functional error in the plotting code.
# The snapshot should be accepted if the overall structure of the plot remains consistent.
test_that("autoplot.barrel_ord works with dbrda", {
  library(ggplot2)
  library(vegan)
  library(vdiffr)

  data(dune)
  data(dune.env)
  set.seed(123)
  ord <- dbrda(dune ~ Management + A1, data = dune.env, distance = "bray")
  ord <- barrel_prepare(ord, dune.env)

  p <- autoplot(
    ord,
    group = "Management",
    data = dune,
    method = "classic",
    kind = "sd",
    geom_type = "polygon",
    show_arrows = TRUE,
    show_labels = FALSE,
    show_centroids = TRUE,
    show_ellipses = TRUE,
    alpha = 0.5
  )

  expect_doppelganger("autoplot barrel_ord (dbrda)", p)
})
