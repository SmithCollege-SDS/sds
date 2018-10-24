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
    dplyr::mutate_(Class = ~as.character(Class),
                   Major1 = ~as.character(Major1))

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
#' term_to_banner_term(c("f18", "s18", "f17"))

term_to_banner_term <- function(x) {
  year <- readr::parse_number(x) + 2000
  semester <- stringr::str_replace_all(x, pattern = "[0-9]+", replacement = "")
  term <- ifelse(semester == "f", (year + 1) * 100 + 1, year * 100 + 3)
  return(term)
}

#' @rdname term_to_banner_term
#' @export
#' @examples
#' banner_term_to_term(c("201901", "201803", "201801"))

banner_term_to_term <- function(x) {
  year <- readr::parse_number(stringr::str_sub(x, 1, 4))
  semester <- stringr::str_sub(x, 5, 6)
  term <- ifelse(semester == "01", paste0("f", year - 2001), paste0("s", year - 2000))
  return(term)
}

#' @rdname term_to_banner_term
#' @param dir Path to directory where BannerWeb export files are located
#' @export
#' @importFrom magrittr %>%
#' @importFrom fs dir_ls path_file path_ext_remove
#' @importFrom stringr str_split_fixed
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate
#' @examples
#' \dontrun{
#' bannerweb_db <- "~/Dropbox/SDS/students/enrollments/bannerweb_exports"
#' crn_from_file(bannerweb_db)
#' }

crn_from_file <- function(dir) {
  crns <- fs::dir_ls(dir, pattern = "ClassList") %>%
    fs::path_file() %>%
    fs::path_ext_remove() %>%
    stringr::str_split_fixed(pattern = "_", n = 6) %>%
    tibble::as_tibble() %>%
    select(-1)
  names(crns) <- c("crn", "dept", "number", "term", "instructor")
  crns %>%
    mutate(filename = paste0(paste("ClassList", crn, dept, number, term, instructor, sep = "_"), ".xls"),
           crn_term = term_to_banner_term(term),
           crn = as.numeric(crn))
}

#' @rdname term_to_banner_term
#' @param path Vector of paths to BannerWeb export files
#' @param domain Domain name for BannerWeb server
#' @param ... currently ignored
#' @importFrom downloader download
#' @importFrom fs path
#' @importFrom purrr walk2
#' @importFrom dplyr mutate
#' @export
#' @examples
#' \dontrun{
#'   paths <- head(fs::dir_ls(bannerweb_db))
#'   file_refresh(paths, "banner.myschool.edu")
#' }

file_refresh <- function(path, domain, ...) {
  filename <- fs::path_file(path)
  parts <- stringr::str_split_fixed(filename, "_", 6)
  crn <- as.numeric(parts[, 2])
  term <- term_to_banner_term(parts[, 5])
  url <- paste0("https://", domain, "/PROD/wroster.P_ExcelList?term=",
                term, "&crn=", crn)
  purrr::walk2(url, fs::path(path),
               ~downloader::download(.x, destfile = .y))
}
