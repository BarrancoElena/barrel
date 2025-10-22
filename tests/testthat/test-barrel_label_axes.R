library(testthat)
library(ggplot2)
library(vegan)

test_that("barrel_label_axes returns a labs object for constrained rda (RDA)", {
  data(dune); data(dune.env)
  ord <- rda(dune ~ Management, data = dune.env)
  labs_layer <- barrel_label_axes(ord)

  expect_true(is_labs(labs_layer))
  expect_true(all(c("x","y") %in% names(labs_layer)))
  expect_true(grepl("^RDA\\d+ \\([^)]*%\\)$", norm_spaces(labs_layer$x)))
  expect_true(grepl("^RDA\\d+ \\([^)]*%\\)$", norm_spaces(labs_layer$y)))
})

test_that("barrel_label_axes works with unconstrained rda (PCA → PC)", {
  data(dune)
  ord <- rda(dune)
  labs_layer <- barrel_label_axes(ord)

  expect_true(is_labs(labs_layer))
  expect_true(all(c("x","y") %in% names(labs_layer)))
  expect_true(grepl("^PC\\d+ \\([^)]*%\\)$", norm_spaces(labs_layer$x)))
  expect_true(grepl("^PC\\d+ \\([^)]*%\\)$", norm_spaces(labs_layer$y)))
})

test_that("barrel_label_axes works with cca (CCA o CA)", {
  data(dune); data(dune.env)
  ord <- cca(dune ~ Management, data = dune.env)
  labs_layer <- barrel_label_axes(ord)

  expect_true(is_labs(labs_layer))
  expect_true(grepl("^(?:CCA|CA)\\d+ \\([^)]*%\\)$", norm_spaces(labs_layer$x)))
  expect_true(grepl("^(?:CCA|CA)\\d+ \\([^)]*%\\)$", norm_spaces(labs_layer$y)))
})

test_that("capscale with constraints returns dbRDA axis labels with variance", {
  data(dune); data(dune.env)
  ord <- capscale(dune ~ Management, data = dune.env, distance = "bray")
  labs <- norm_spaces(get_ord_axis_labels(ord)$label)
  expect_true(all(grepl("^dbRDA[12] \\([^)]*%\\)$", labs[1:2])))
})

test_that("barrel_label_axes works with capscale (dbRDA, CAP o PC)", {
  data(dune)
  ord <- capscale(dune ~ 1, distance = "bray")
  labs_layer <- barrel_label_axes(ord)

  expect_true(is_labs(labs_layer))
  expect_true(grepl("^(?:dbRDA|CAP|PC)\\d+ \\([^)]*%\\)$", norm_spaces(labs_layer$x)))
  expect_true(grepl("^(?:dbRDA|CAP|PC)\\d+ \\([^)]*%\\)$", norm_spaces(labs_layer$y)))
})

test_that("barrel_label_axes works with metaMDS (NMDS)", {
  data(dune)
  set.seed(123)
  ord <- metaMDS(dune, k = 2, trymax = 20, trace = 0)
  labs_layer <- barrel_label_axes(ord)

  expect_true(is_labs(labs_layer))
  expect_equal(labs_layer$x, "NMDS1")
  expect_equal(labs_layer$y, "NMDS2")
})

test_that("barrel_label_axes works with decorana (DCA)", {
  data(dune)
  ord <- decorana(dune)
  labs_layer <- barrel_label_axes(ord)

  expect_true(is_labs(labs_layer))
  expect_equal(labs_layer$x, "DCA1")
  expect_equal(labs_layer$y, "DCA2")
})

test_that("barrel_label_axes fails with unsupported ordination class", {
  fake_ord <- list()
  class(fake_ord) <- "unknown"
  expect_error(
    barrel_label_axes(fake_ord),
    regexp = "(?i)(cannot\\s*find\\s*scores|unsupported)"
  )
})
