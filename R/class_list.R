#' Combine class BannerWeb class lists into one
#' @param xls character vector of paths to XLS files exported from BannerWeb
#' @export
#' @importFrom xml2 read_html
#' @importFrom rvest html_table
#' @importFrom magrittr extract2 %>%
#' @importFrom dplyr filter mutate bind_rows
#' @importFrom tibble repair_names
#'
#' @examples
#' \dontrun{
#' dir <- "~/Dropbox/SDS/students/enrollments/bannerweb_exports"
#' docs <- list.files(dir, pattern = "291", full.names = TRUE)
#' my_list <- class_list(docs)
#' dim(my_list)
#' write.csv(my_list, file = "my_class.csv", row.names = FALSE)
#' }

class_list <- function(xls) {
  lapply(xls, class_list_one) %>%
    dplyr::bind_rows()
}

class_list_one <- function(xls) {
  if (!file.exists(xls)) {
    stop("Can't find that file!")
  }
  # Note that the XLS files exported from BannerWeb are not actually Excel files!
  # They are malformed HTML tables!
  out <- xml2::read_html(xls) %>%
    rvest::html_table(fill = TRUE, header = TRUE) %>%
    magrittr::extract2(1) %>%
    repair_names() %>%
    dplyr::rename_("Student" = ~`Student Name*`) %>%
    dplyr::filter_(~!is.na(ID)) %>%
    dplyr::mutate_(Class = ~as.character(Class))

  if (ncol(out) == 11) {
    # shift columns for concentrations
    names(out)[9:11] <- names(out)[8:10]
    names(out)[7:8] <- c("Major2", "concentration")
  }
  return(out)
}


#' Utilities for dealing with BannerWeb
#' @param x Description of a term
#' @export
#' @examples
#' term_to_banner_term("f18")
#' term_to_banner_term("s18")

term_to_banner_term <- function(x) {
  year <- readr::parse_number(x) + 2000
  semester <- gsub("[0-9]", "", x)
  if (semester == "f") {
    term <- (year + 1) * 100 + 1
  } else {
    term <- year * 100 + 3
  }
  return(term)
}

#' @rdname term_to_banner_term
#' @export
#' @examples
#' banner_term_to_term("201901")
#' banner_term_to_term("201803")

banner_term_to_term <- function(x) {
  year <- readr::parse_number(stringr::str_sub(x, 1, 4))
  semester <- stringr::str_sub(x, 5, 6)
  if (semester == "01") {
    term = paste0("f", year - 2001)
  } else {
    term = paste0("s", year - 2000)
  }
  return(term)
}
