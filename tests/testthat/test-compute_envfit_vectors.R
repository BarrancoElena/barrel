library(testthat)

test_that("compute_envfit_vectors returns vectors filtered by significance", {
  library(vegan)

  data(dune)
  data(dune.env)
  rda_mod <- rda(dune ~ A1 + Moisture, data = dune.env)

  all_vecs <- compute_envfit_vectors(rda_mod, dune.env, p_thresh = 0.05, show.significant = FALSE)
  sig_vecs <- compute_envfit_vectors(rda_mod, dune.env, p_thresh = 0.05, show.significant = TRUE)

  expect_true(nrow(all_vecs) >= nrow(sig_vecs))
  expect_true(all(sig_vecs$pval <= 0.05))
})
