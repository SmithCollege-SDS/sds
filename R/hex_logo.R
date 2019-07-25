globalVariables(c("inside_lwr", "inside_upr", "profile.email",
                  "r", "r_lwr", "r_upr", "real_name", "subtype",
                  "user", "x.var", "y.var"))

#' Print the Smith SDS hex logo
#' @importFrom graphics lines par plot points polygon segments text
#' @importFrom stats lm predict rnorm
#' @importFrom grDevices png dev.off
#' @param file file path
#' @param ... arguments passed to \code{\link{png}}
#' @export
#' @author Miles Ott
#' @examples
#' hex_logo()


hex_logo <- function(file = "sds_hex.png", ...) {

  grDevices::png(file = file, width = 480, height = 480, units = "px",
                 res = 300, family = "Courier", bg = "transparent", ...)

  par(mar = c(0,0,0,0), bty = "n")

  # setting the plotting area
  plot(NULL, xlim = c(-1,1), ylim = c(-1,1), axes = FALSE)

  # drawing the border
  x <- c(sqrt(.75), 0, -(sqrt(.75)), -(sqrt(.75)), 0, sqrt(.75))
  y <- c(.5, 1, .5, -.5, -1, -.5)
  polygon(x, y, lwd = 4, col = "white", border = "#002855")

  # fill color
  polygon(x = x*.93, y = y*.93, col = "#F2A900", border = "white")

  # plotting the scatterplot and regression line
  set.seed(3)
  points <- tibble::tibble(
    x.var = c(rnorm(20, 0, .2), -1, 1),
    y.var = x.var + rnorm(22, 0, .25),
    r = sqrt(x.var^2 + y.var^2),
    inside = r < 1
  )
  fit <- lm(y.var ~ x.var, data = points)

  inside <- points %>%
    dplyr::filter(inside)
  points(inside$x.var, y = inside$y.var, col = "white", pch = 16, cex = .5)
  edge <- 0.6
  segments(x0 = -edge, y0 = -edge, x1 = edge, y1 = edge, lwd = 1.5, col = "white")

  # plotting the prediction intervals
  grid <- data.frame(x.var = seq(-1, 1, length = nrow(points)))
  pred <- predict(
    fit, interval = "prediction",
    newdata = grid
  ) %>%
    tibble::as_tibble() %>%
    mutate(x.var = grid$x.var,
           r_lwr = sqrt(x.var^2 + lwr^2),
           r_upr = sqrt(x.var^2 + upr^2),
           inside_lwr = r_lwr < 0.9,
           inside_upr = r_upr < 0.9) %>%
    arrange(x.var)

  lwr <- pred %>%
    filter(inside_lwr)
  lines(lwr$x.var, lwr$lwr, lty = 2, lwd = 1.5, col = "white")

  upr <- pred %>%
    filter(inside_upr)
  lines(upr$x.var, upr$upr, lty = 2, lwd = 1.5, col = "white")

  # replotting the border
  polygon(x, y, lwd = 4, border = "#002855", col = NULL)

  # text
  text(x = 0, y = .45, "Smith College", col = "#002855", cex = .78, font = 2)
  text(x = 0, y = 0, "SDS", col = "#002855", cex = 3.4, font = 2)
  text(x = 0, y = -.45, "Statistical &", col = "#002855", cex = .5, font = 2)
  text(x = 0, y = -.58, "Data Sciences", col = "#002855", cex = .5, font = 2)

  grDevices::dev.off()
}

#' @rdname hex_logo
#' @export
#' @examples
#' img_logo()
#' img_logo(width = 64)

img_logo <- function(...) {
  uri <- knitr::image_uri(system.file('sds_hex.png', package = 'sds'))
  htmltools::img(src = uri, ...)
}

