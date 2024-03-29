test_that("p_to_bf works", {
  skip_if_not_or_load_if_installed("parameters")

  m <- lm(mpg ~ hp + cyl + am, data = mtcars)
  p <- coef(summary(m))[-1, 4]

  # BF by hand
  bfs <- 3 * p * sqrt(insight::n_obs(m))

  expect_equal(p_to_bf(m, log = FALSE)[-1, ]$BF, exp(-log(bfs)), tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(p_to_bf(m, log = TRUE)[-1, ]$log_BF, -log(bfs), tolerance = 1e-4, ignore_attr = TRUE)
})
