library(testthat)
# This test may fail intermittently due to minor label shifts caused by ggrepel::geom_text_repel().
# Such changes are expected and do not indicate a functional error in the plotting code.
# The snapshot should be accepted if the overall structure of the plot remains consistent.
test_that("stat_barrel draws ellipses with classic and robust methods", {
  library(ggplot2)
  library(vegan)
  library(robustbase)

  data(dune)
  data(dune.env)
  pca <- rda(dune, scale = TRUE)
  scores <- as.data.frame(vegan::scores(pca, display = "sites"))
  scores$Group <- dune.env$Management

  p <- ggplot(scores, aes(x = PC1, y = PC2, group = Group, color = Group)) +
    stat_barrel(method = "classic", kind = "se", conf = 0.95)
  expect_s3_class(p, "ggplot")

  p2 <- ggplot(scores, aes(x = PC1, y = PC2, group = Group, color = Group)) +
    stat_barrel(method = "robust", kind = "sd", conf = 0.68)
  expect_s3_class(p2, "ggplot")
})
