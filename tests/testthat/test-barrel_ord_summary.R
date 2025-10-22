library(testthat)
library(vegan)

test_that("get_ord_summary returns stress for metaMDS", {
  data(dune)
  ord <- metaMDS(dune, k = 2)
  out <- barrel_ord_summary(ord)

  expect_equal(out$method, "NMDS")
  expect_true("stress" %in% names(out))
  expect_type(out$stress, "double")
  expect_true(out$stress > 0)
  expect_match(out$message, "R² and axis variance not available")
})

test_that("get_ord_summary returns method and axis variance for unconstrained rda", {
  data(dune)
  ord <- rda(dune)
  out <- barrel_ord_summary(ord)

  expect_equal(out$method, "rda")
  expect_true(is.na(out$R2))
  expect_named(out$axis_var)
  expect_true(all(grepl("^Axis\\d+", names(out$axis_var))))
})

test_that("get_ord_summary works for cca", {
  data(dune)
  data(dune.env)
  ord <- cca(dune ~ Management, data = dune.env)
  out <- barrel_ord_summary(ord)

  expect_equal(out$method, "cca")
  expect_type(out$R2, "double")
  expect_type(out$R2_adj, "double")
})

test_that("barrel_ord_summary works for capscale", {
  data(dune)
  ord <- capscale(dune ~ 1, distance = "bray")
  out <- barrel_ord_summary(ord)

  expect_equal(out$method, "capscale")
  expect_type(out$R2, "double")
  expect_named(out$axis_var)
})
