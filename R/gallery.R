#' Make a gallery of HTML docs downloaded from Moodle
#' @export
#' @param from directory of Moodle assignment
#' @param to directory of new gallery
#' @param ... currently ignored
#' @importFrom tools md5sum
#' @importFrom tibble as.tibble rownames_to_column
#' @importFrom dplyr mutate
#'
#' @examples
#' \dontrun{
#' my_dir <- "~/Dropbox/git/sds192/student_info/SDS192- 01_201801-Mini-project #4-265952/"
#' new_dir <- "~/Dropbox/git/sds192/www/gallery"
#' if (require(dplyr)) {
#'   my_dir %>%
#'     cull_html(to = new_dir) %>%
#'     anonymize()
#' }
#' }
#'

cull_html <- function(from, to, ...) {
  originals <- list.files(from, recursive = TRUE,
                          pattern = "*.html", full.names = TRUE)
  x <- originals %>%
    tools::md5sum() %>%
    tibble::as.tibble() %>%
    tibble::rownames_to_column(var = "path") %>%
    dplyr::mutate_(new_filename = ~paste0(value, ".html"),
                   new_path = ~file.path(to, new_filename))

  file.copy(from = originals, to = x$new_path)
  return(to)
}

#' @rdname cull_html
#' @param dir gallery directory
#' @export

anonymize <- function(dir) {
  sapply(list.files(dir, pattern = "*.html", full.names = TRUE),
         html_remove_byline) %>%
    unlist()
}

#' @rdname cull_html
#' @export
#' @importFrom rvest html_attr html_nodes
#' @importFrom xml2 read_html xml_remove write_xml
#' @param path file to be anonymized
#' @param verbose tell you what happened?

html_remove_byline <- function(path, verbose = FALSE, ...) {
  page <- path %>%
    xml2::read_html()

  # find authors name in meta
  meta <- page %>%
    rvest::html_nodes("meta")
  y <- meta %>%
    rvest::html_attr("name") == "author"
  if (any(y, na.rm = TRUE)) {
    if (verbose) {
      message(paste("Deleting meta information from", path))
    }
    xml2::xml_remove(meta)
  } else {
    message(paste("Didn't find author meta information in", path))
  }

  # find byline
  byline <- page %>%
    rvest::html_nodes(".author")
  if (length(byline) > 0) {
    if (verbose) {
      message(paste("...deleting byline from", path))
    }
    xml2::xml_remove(byline)
  } else {
    message(paste("...no byline found in", path))
  }

  xml2::write_xml(page, path)
  rm(page)
}
