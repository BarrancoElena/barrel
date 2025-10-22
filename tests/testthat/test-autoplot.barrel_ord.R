library(testthat)

test_that("autoplot.barrel_ord returns a ggplot object", {
  skip_if_not_installed("ggplot2")
  library(vegan)
  data(dune)
  data(dune.env)
  set.seed(123)
  ord <- rda(dune, scale = TRUE)
  ord <- barrel_prepare(ord, dune.env)

  p <- ggplot2::autoplot(ord,
    group = "Management", data = dune,
    method = "classic", kind = "se", geom_type = "polygon",
    show_arrows = FALSE, show_centroids = TRUE, show_ellipses = TRUE
  )

  expect_s3_class(p, "ggplot")
})

test_that("autoplot.barrel_ord returns a ggplot object", {
  library(vegan)
  library(ggplot2)
  data(dune)
  data(dune.env)

  ord <- rda(dune, scale = TRUE)
  ord <- barrel_prepare(ord, dune.env)

  p <- autoplot(ord, group = "Management", show_arrows = FALSE, show_centroids = FALSE, show_ellipses = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("autoplot.barrel_ord errors when grouping variable is missing", {
  library(vegan)
  data(dune)
  data(dune.env)

  # Crear metadata sin la variable "Management"
  fake_meta <- data.frame(dummy = dune.env$Use, stringsAsFactors = FALSE)

  # Preparar objeto barrel_ord con metadata incorrecta
  ord <- rda(dune)
  ord <- barrel_prepare(ord, fake_meta)

  # Llamar a autoplot() con group = "Management" debería fallar
  expect_error(
    autoplot(ord, group = "Management"),
    regexp = "Grouping variable not found in ordination metadata"
  )
})
