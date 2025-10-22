library(testthat)

test_that("ord_compute_ellipse returns correct ellipse coordinates", {
  cov_mat <- diag(2)
  center <- c(0, 0)

  ell_se <- ord_compute_ellipse(cov_mat, center, kind = "se", conf = 0.95, n = 10)
  expect_equal(dim(ell_se), c(101, 2))
  expect_true(all(is.finite(ell_se)))

  ell_sd <- ord_compute_ellipse(cov_mat, center, kind = "sd", conf = 0.68)
  expect_equal(dim(ell_sd), c(101, 2))
})

test_that("ord_compute_ellipse errors for invalid kind or missing n", {
  cov_mat <- diag(2)
  expect_error(ord_compute_ellipse(cov_mat, c(0, 0), kind = "invalid"))
  expect_error(ord_compute_ellipse(cov_mat, c(0, 0), kind = "se"))
})
