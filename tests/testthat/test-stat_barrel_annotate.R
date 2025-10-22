library(testthat)

test_that("stat_barrel_annotate adds annotation layer", {
  library(ggplot2)
  library(vegan)

  data(dune)
  data(dune.env)
  rda_mod <- rda(dune ~ A1 + Moisture, data = dune.env)
  scores <- as.data.frame(vegan::scores(rda_mod, display = "sites"))
  scores$Group <- dune.env$Management

  p <- ggplot(scores, aes(x = RDA1, y = RDA2)) +
    stat_barrel_annotate(ord = rda_mod)
  expect_s3_class(p, "ggplot")
})
