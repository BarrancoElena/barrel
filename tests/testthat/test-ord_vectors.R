library(testthat)

test_that("ord_vectors filters vectors correctly based on p-value", {
  library(vegan)
  data(dune)
  data(dune.env)

  ord <- rda(dune)

  sig_vecs <- ord_vectors(ord, dune, p_thresh = 0.05, filter = "significant")
  nonsig_vecs <- ord_vectors(ord, dune, p_thresh = 0.05, filter = "nonsignificant")
  all_vecs <- ord_vectors(ord, dune, filter = "all")

  expect_true(all(sig_vecs$pval <= 0.05))
  expect_true(all(nonsig_vecs$pval > 0.05))
  expect_true(nrow(all_vecs) >= nrow(sig_vecs))
})
