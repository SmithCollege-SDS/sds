#' Smith College color palettes
#'
#' @param reverse Reverse the order of colors?
#' @param ... arguments passed to methods
#' @references \url{https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2}
#' @examples
#'
#' if (require(ggplot2)) {
#'   ggplot(mtcars, aes(x = disp, y = mpg)) +
#'     geom_point(aes(color = mpg)) +
#'     scale_color_smith_c(reverse = TRUE)
#'
#'   ggplot(mtcars, aes(x = disp, y = mpg)) +
#'     geom_point(aes(color = mpg)) +
#'     scale_color_smith_cd(midpoint = 20)
#'
#'   ggplot(mtcars, aes(x = disp, y = mpg)) +
#'     geom_point(aes(color = factor(am))) +
#'     scale_color_smith_d()
#' }
#' @export
#' @importFrom ggplot2 scale_color_gradientn
#' @importFrom grDevices colorRampPalette

scale_color_smith_c <- function(reverse = FALSE, ...) {
  colors <- grDevices::colorRampPalette(colors = smith_colors)(256)
  if (reverse) {
    colors <- rev(colors)
  }
  ggplot2::scale_color_gradientn(colours = colors, ...)
}

#' @rdname scale_color_smith_c
#' @export
#' @importFrom ggplot2 scale_color_gradient2

scale_color_smith_cd <- function(reverse = FALSE, ...) {
  ends <- smith_colors[1:2]
  if (reverse) {
    ends <- rev(ends)
  }
  ggplot2::scale_color_gradient2(high = ends[1], low = ends[2], mid = "white", ...)
}

#' @rdname scale_color_smith_c
#' @export
#' @importFrom ggplot2 discrete_scale

scale_color_smith_d <- function(reverse = FALSE, ...) {
  ggplot2::discrete_scale("color", "smith",
                          palette = c(smith_colors[1], smith_colors[2], "black", "white"), ...)
}



smith_colors <- c("#002855", "#F2A900")

