#' Smith College color palettes
#'
#' @param reverse Reverse the order of colors?
#' @param ... arguments passed to methods
#' @references \url{https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2}
#' @description Smith College themed \code{ggplot2} color palettes
#' @examples
#'
#' if (require(ggplot2)) {
#'   x <- ggplot(mtcars, aes(x = disp, y = mpg, color = mpg)) +
#'     geom_point()
#'   x + scale_color_smith_c(reverse = TRUE)
#'   x + scale_color_smith_cd(midpoint = 20)
#'   x + aes(color = factor(am)) + scale_color_smith_d()
#'   x + aes(color = factor(gear)) + scale_color_smith_d()
#'   x + aes(color = factor(carb)) + scale_color_smith_d()
#'   x + aes(color = factor(am)) + scale_fill_smith_d()
#'
#'   y <- ggplot(mtcars, aes(x = cyl, fill = factor(am))) + geom_bar(position = "dodge")
#'
#'   ggplot(mpg, aes(y = hwy, x = reorder(model, hwy), color = displ, fill = hwy)) +
#'     geom_col(position = "dodge") +
#'     coord_flip() +
#'     facet_wrap(~year) +
#'     scale_fill_smith_cd(midpoint = 20) +
#'     scale_color_smith_c()
#' }
#' @export

scale_color_smith_c <- function(reverse = FALSE, ...) {
  colors <- grDevices::colorRampPalette(colors = smith_colors)(256)
  if (reverse) {
    colors <- rev(colors)
  }
  ggplot2::scale_color_gradientn(colours = colors, ...)
}

#' @rdname scale_color_smith_c
#' @export

scale_fill_smith_c <- function(reverse = FALSE, ...) {
  colors <- grDevices::colorRampPalette(colors = smith_colors)(256)
  if (reverse) {
    colors <- rev(colors)
  }
  ggplot2::scale_fill_gradientn(colours = colors, ...)
}


#' @rdname scale_color_smith_c
#' @export

scale_color_smith_cd <- function(reverse = FALSE, ...) {
  ends <- smith_colors[1:2]
  if (reverse) {
    ends <- rev(ends)
  }
  ggplot2::scale_color_gradient2(high = ends[1], low = ends[2], mid = "white", ...)
}

#' @rdname scale_color_smith_c
#' @export

scale_fill_smith_cd <- function(reverse = FALSE, ...) {
  ends <- smith_colors[1:2]
  if (reverse) {
    ends <- rev(ends)
  }
  ggplot2::scale_fill_gradient2(high = ends[1], low = ends[2], mid = "white", ...)
}


#' @rdname scale_color_smith_c
#' @export

scale_color_smith_d <- function(...) {
  ggplot2::discrete_scale("colour", "smith",
                          palette = smith_pal_categorical, ...)
}


#' @rdname scale_color_smith_c
#' @export

scale_fill_smith_d <- function(...) {
  ggplot2::discrete_scale("fill", "smith",
                          palette = smith_pal_categorical, ...)
}


#' @rdname scale_color_smith_c
#' @param n number of colors desired
#' @export
#' @examples
#' smith_pal_categorical(2)

smith_pal_categorical <- function(n) {
  c(smith_colors[1], smith_colors[2], "gray60", "black", "white")[1:n]
}

smith_colors <- c("#002855", "#F2A900")

