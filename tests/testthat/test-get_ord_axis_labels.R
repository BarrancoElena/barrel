library(testthat)
library(vegan)

test_that("get_ord_axis_labels returns a list with 'label'", {
  data(dune)
  ord <- rda(dune, scale = TRUE)
  out <- get_ord_axis_labels(ord)

  expect_type(out, "list")
  expect_named(out, "label")
  expect_true(is.character(out$label))
  expect_length(out$label, 2)
})

test_that("RDA labels contain method name and percent", {
  data(dune); data(dune.env)
  ord <- rda(dune ~ Management, data = dune.env)  # << antes usabas rda(dune)
  labels <- get_ord_axis_labels(ord)$label

  expect_true(grepl("^RDA1 \\([^)]*%\\)$", labels[1]))
  expect_true(grepl("^RDA2 \\([^)]*%\\)$", labels[2]))
})

test_that("NMDS labels are generic and do not contain variance", {
  data(dune)
  ord <- metaMDS(dune, k = 2)
  labels <- get_ord_axis_labels(ord)$label

  expect_equal(labels, c("NMDS1", "NMDS2"))
})

test_that("capscale returns dbRDA axis labels with variance", {
  data(dune)
  ord <- capscale(dune ~ 1, distance = "bray")
  labels <- get_ord_axis_labels(ord)$label

  expect_true(all(grepl("^CAP[12] \\(.*%\\)$", labels)))
})

test_that("get_ord_axis_labels returns method-prefixed axis labels with %", {
  data(dune)
  data(dune.env)
  ord <- cca(dune ~ Management, data = dune.env)
  labels <- get_ord_axis_labels(ord)$label

  method_prefix <- sub("\\d+.*", "", labels[1])

  expect_true(all(grepl(paste0("^", method_prefix, "\\d+ \\([0-9.]+%\\)$"), labels)))
})

test_that("get_ord_axis_labels errors on unknown object class", {
  fake_ord <- list()
  class(fake_ord) <- "unknown"

  expect_error(get_ord_axis_labels(fake_ord), regexp = "cannot find scores")
})

test_that("get_ord_axis_labels uses fallback method 'Axis'", {
  dummy <- list()
  class(dummy) <- "unknown"
  local({
    scores.unknown <- function(ord, display = "sites") {
      matrix(1:4, ncol = 2)
    }
    environment(scores.unknown) <- globalenv()
    assign("scores.unknown", scores.unknown, envir = globalenv())
    result <- get_ord_axis_labels(dummy)
    expect_equal(result$label, c("Axis1", "Axis2"))
    rm(scores.unknown, envir = globalenv())
  })
})

test_that("unconstrained rda returns PC axis labels with variance", {
  data(dune)
  ord <- rda(dune)  # PCA sin restricciones
  lab <- get_ord_axis_labels(ord)$label
  expect_true(grepl("^PC1 \\([^)]*%\\)$", lab[1]))
  expect_true(grepl("^PC2 \\([^)]*%\\)$", lab[2]))
})

test_that("cca without constraints returns CA axis labels with variance", {
  data(dune)
  ord <- cca(dune)  # CA (sin término de restricción)
  lab <- get_ord_axis_labels(ord)$label
  expect_true(grepl("^CA1 \\([^)]*%\\)$", lab[1]))
  expect_true(grepl("^CA2 \\([^)]*%\\)$", lab[2]))
})

test_that("capscale with constraints returns dbRDA axis labels with variance", {
  data(dune); data(dune.env)
  ord <- capscale(dune ~ Management, data = dune.env, distance = "bray")
  lab <- get_ord_axis_labels(ord)$label
  expect_true(grepl("^dbRDA1 \\([^)]*%\\)$", lab[1]))
  expect_true(grepl("^dbRDA2 \\([^)]*%\\)$", lab[2]))
})

test_that("decorana returns DCA axis labels (no variance shown)", {
  data(dune)
  ord <- decorana(dune)
  lab <- get_ord_axis_labels(ord)$label
  expect_true(grepl("^DCA1$", lab[1]))
  expect_true(grepl("^DCA2$", lab[2]))
})
