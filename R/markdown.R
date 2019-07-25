
#' Class notes format (PDF)
#'
#' @inheritParams rmarkdown::pdf_document
#' @import rmarkdown
#' @import knitr
#' @export
#' @examples
#' class_notes()
#'
class_notes <- function(fig_width = 10,
                          fig_height = 2.5,
                          fig_crop = TRUE,
                          dev = 'pdf',
                          highlight = "default",
                          keep_tex = FALSE,
                          includes = NULL,
                          md_extensions = NULL,
                          pandoc_args = NULL) {

  # resolve default highlight
  if (identical(highlight, "default"))
    highlight <- "pygments"

  # get the template
  template <-  system.file("rmarkdown", "templates", "class_notes/resources/class_notes.tex", package = "sds")

  # call the base pdf_document format with the appropriate options
  format <- rmarkdown::pdf_document(fig_width = fig_width,
                                    fig_height = fig_height,
                                    fig_crop = fig_crop,
                                    dev = dev,
                                    highlight = highlight,
                                    template = template,
                                    keep_tex = keep_tex,
                                    latex_engine = "pdflatex",
                                    includes = includes,
                                    md_extensions = md_extensions,
                                    pandoc_args = pandoc_args)


  # create knitr options (ensure opts and hooks are non-null)
  knitr_options <- knitr_options_pdf(fig_width, fig_height, fig_crop, dev)
  if (is.null(knitr_options$opts_knit))
    knitr_options$opts_knit <- list()
  if (is.null(knitr_options$knit_hooks))
    knitr_options$knit_hooks <- list()

  # set options
  knitr_options$opts_chunk$tidy <- TRUE
  knitr_options$opts_knit$width <- 45

  # set hooks for special plot output
  knitr_options$knit_hooks$plot <- function(x, options) {

    # determine caption (if any)
    caption <- ifelse(is.null(options$fig.cap),
                      "",
                      paste("\\caption{", options$fig.cap, "}\n", sep = ""))

    # determine figure type
    if (isTRUE(options$fig.margin))
      figtype <- "marginfigure"
    else if (isTRUE(options$fig.fullwidth))
      figtype <- "figure*"
    else
      figtype <- "figure"

    # return the latex
    paste(sprintf('\\begin{%s}\n \\includegraphics{%s}\n%s\\end{%s}\n',
                  figtype, x, caption, figtype))
  }

  # override the knitr settings of the base format and return the format
  format$knitr <- knitr_options
  format
}


#' Smith letter stationery (PDF)
#' @description Write a letter in R Markdown on the official Smith College SDS
#' letterhead
#' @details Note that this uses \code{ebgaramond-maths} as the default font. You
#' will need the \code{texlive-fonts-extra} package or the equivalent. Note also
#' that in order for this to work you must us the \code{xelatex} rendering engine.
#' @inheritParams rmarkdown::pdf_document
#' @export
#' @seealso \code{\link[rmarkdown]{pdf_document}}
#' @source \url{https://www.smith.edu/about-smith/college-relations/visual-identity-program}
#' @examples
#' \dontrun{
#' # simple invocation
#' render("input.Rmd", sds::letter())
#' }
#'
#'
letter <- function(fig_width = 10,
                   fig_height = 5,
                   fig_crop = TRUE,
                   dev = 'pdf',
                   highlight = "default",
                   keep_tex = FALSE,
                   includes = NULL,
                   md_extensions = NULL,
                   pandoc_args = NULL, ...) {

  # get the template
  template <-  system.file("rmarkdown", "templates", "letter/resources/template.tex", package = "sds")
  letterhead <-  system.file("rmarkdown", "templates", "letter/resources/stationery.pdf", package = "sds")
  signature <-  system.file("rmarkdown", "templates", "letter/resources/signature.pdf", package = "sds")
  pandoc_args <- paste0(pandoc_args,
                        "--variable=", "letterhead:", letterhead)

  # call the base pdf_document format with the appropriate options
  rmarkdown::pdf_document(fig_width = fig_width,
                          fig_height = fig_height,
                          fig_crop = fig_crop,
                          dev = dev,
                          highlight = highlight,
                          template = template,
                          keep_tex = keep_tex,
                          latex_engine = "xelatex",
                          includes = includes,
                          md_extensions = md_extensions,
                          pandoc_args = pandoc_args, ...)
}



#' Smith memo stationery (PDF)
#'
#' @inheritParams rmarkdown::pdf_document
#' @import rmarkdown
#' @import knitr
#' @export
#' @examples
#' letter()
#'
memo <- function(fig_width = 10,
                   fig_height = 2.5,
                   fig_crop = TRUE,
                   dev = 'pdf',
                   highlight = "default",
                   keep_tex = FALSE,
                   includes = NULL,
                   md_extensions = NULL,
                   pandoc_args = NULL) {

  # resolve default highlight
  if (identical(highlight, "default"))
    highlight <- "pygments"

  # get the template
  template <-  system.file("rmarkdown", "templates", "memo/resources/template.tex", package = "sds")
  letterhead <-  system.file("rmarkdown", "templates", "memo/resources/memoform.pdf", package = "sds")

  # call the base pdf_document format with the appropriate options
  format <- rmarkdown::pdf_document(fig_width = fig_width,
                                    fig_height = fig_height,
                                    fig_crop = fig_crop,
                                    dev = dev,
                                    highlight = highlight,
                                    template = template,
                                    keep_tex = keep_tex,
                                    latex_engine = "pdflatex",
                                    includes = includes,
                                    md_extensions = md_extensions,
                                    pandoc_args = pandoc_args)


  # create knitr options (ensure opts and hooks are non-null)
  knitr_options <- knitr_options_pdf(fig_width, fig_height, fig_crop, dev)
  if (is.null(knitr_options$opts_knit))
    knitr_options$opts_knit <- list()
  if (is.null(knitr_options$knit_hooks))
    knitr_options$knit_hooks <- list()

  # set options
  knitr_options$opts_chunk$tidy <- TRUE
  knitr_options$opts_knit$width <- 45

  # set hooks for special plot output
  knitr_options$knit_hooks$plot <- function(x, options) {

    # determine caption (if any)
    caption <- ifelse(is.null(options$fig.cap),
                      "",
                      paste("\\caption{", options$fig.cap, "}\n", sep = ""))

    # determine figure type
    if (isTRUE(options$fig.margin))
      figtype <- "marginfigure"
    else if (isTRUE(options$fig.fullwidth))
      figtype <- "figure*"
    else
      figtype <- "figure"

    # return the latex
    paste(sprintf('\\begin{%s}\n \\includegraphics{%s}\n%s\\end{%s}\n',
                  figtype, x, caption, figtype))
  }

  # override the knitr settings of the base format and return the format
  format$knitr <- knitr_options
  format
}



#' Smith xaringan slides (HTML)
#' @description Write a presentation in R Markdown using Smith College SDS theming
#' @details This function is a thin wrapper around \code{\link[xaringan]{moon_reader}}
#' @inheritParams xaringan::moon_reader
#' @export
#' @seealso \code{\link[xaringan]{moon_reader}}
#' @source \url{https://www.smith.edu/about-smith/college-relations/visual-identity-program}
#' @examples
#' \dontrun{
#' # simple invocation
#' rmarkdown::render("input.Rmd", "sds::xaringan")
#' }
#'
#'
moon_reader <- function(css = c("default", "default-fonts"),
                     self_contained = FALSE,
                     seal = TRUE, yolo = FALSE,
                     chakra = "https://remarkjs.com/downloads/remark-latest.min.js",
                     nature = list(), ...) {

  smith_nature <- list(
    beforeInit = system.file("rmarkdown", "templates", "xaringan", "resources", "macros.js", package = "sds"))

  # call the base moon_reader format with the appropriate options
  xaringan::moon_reader(
    css = css, self_contained, seal, yolo, chakra,
    nature = append(smith_nature, nature), ...)

}

