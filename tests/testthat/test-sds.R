context("sds")

test_that("colors works", {
  library(ggplot2)
  x <- ggplot(mtcars, aes(x = disp, y = mpg, color = mpg)) +
    geom_point()
  expect_s3_class(x + scale_color_smith_c(reverse = TRUE), "ggplot")
  expect_s3_class(x + scale_color_smith_cd(midpoint = 20), "ggplot")
  expect_s3_class(x + aes(color = factor(am)) + scale_color_smith_d(), "ggplot")
  expect_s3_class(x + aes(fill = factor(am)) + scale_fill_smith_d(), "ggplot")

})
