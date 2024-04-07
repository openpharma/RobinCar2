test_that("predict_counterfactual works for guassian", {
  fit <- glm(Sepal.Length ~ Sepal.Width + Species, data = iris)
  expect_snapshot(predict_counterfactual(fit, "Species"))
})

test_that("predict_counterfactual works for guassian with lm", {
  fit <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
  expect_snapshot(predict_counterfactual(fit, "Species", data = iris))
})

test_that("predict_counterfactual works for binomial", {
  grow <- ToothGrowth
  grow$resp <- ifelse(grow$len < 10, 0, 1)
  fit <- glm(resp ~ supp + dose, data = grow)
  expect_snapshot(predict_counterfactual(fit, "supp"))
})
