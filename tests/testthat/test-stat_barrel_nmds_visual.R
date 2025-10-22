test_that("stat_barrel layers work with metaMDS", {
  library(ggplot2)
  library(vegan)
  library(vdiffr)

  data(dune)
  data(dune.env)
  set.seed(123)
  ord <- metaMDS(dune, k = 2)
  ord <- barrel_prepare(ord, dune.env)
  scores <- as.data.frame(scores(ord, display = "sites"))
  scores$Management <- dune.env$Management

  p <- ggplot(scores, aes(x = NMDS1, y = NMDS2, group = Management, fill = Management)) +
    stat_barrel(
      method = "classic", kind = "sd", conf = 0.95,
      geom_type = "polygon", alpha = 0.4, color = "black"
    ) +
    stat_barrel_centroid(method = "classic", shape = 3) +
    stat_barrel_arrows(
      ord = ord, matrix = dune, labels = TRUE,
      labels.color = "blue", arrow.color = "darkred",
      arrow.linetype = "solid", labels.fontface = "bold",
      show.significant = TRUE
    ) +
    stat_barrel_annotate(ord = ord, xpad = 0.05, ypad = 0.05) +
    geom_point(aes(color = Management)) +
    theme_minimal()

  expect_doppelganger("stat_barrel with metaMDS", p)
})
