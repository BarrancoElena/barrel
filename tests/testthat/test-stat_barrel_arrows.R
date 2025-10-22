library(testthat)

test_that("stat_barrel_arrows creates arrow layers", {
  library(ggplot2)
  library(vegan)
  library(ggrepel)

  data(dune)
  data(dune.env)
  rda_mod <- rda(dune ~ A1 + Moisture, data = dune.env)
  scores <- as.data.frame(vegan::scores(rda_mod, display = "sites"))

  layers <- stat_barrel_arrows(ord = rda_mod, matrix = dune.env, labels = TRUE)
  expect_true(is.list(layers))
  expect_true(all(vapply(layers, function(x) inherits(x, "LayerInstance"), logical(1))))
})
