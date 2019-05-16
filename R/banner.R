globalVariables(c("crn", "dept", "number", "term", "instructor", "xpos_36",
                  "xpos_163", "xpos_179", "xpos_594", "xpos_205", "xpos_226",
                  "xpos_256", "xpos_363", "xpos_397", "xpos_507",
                  "name_first", "name_last", "adviser1", "adviser2",
                  "major1", "major2"))

#' Combine class BannerWeb class lists into one
#' @param path character vector of paths to XLS files exported from BannerWeb
#' @export
#'
#' @examples
#' \dontrun{
#' dir <- "~/Dropbox/SDS/students/enrollments/bannerweb_exports"
#' docs <- list.files(dir, pattern = "291", full.names = TRUE)
#' my_list <- read_banner_course(docs)
#' dim(my_list)
#' write.csv(my_list, file = "my_class.csv", row.names = FALSE)
#' }

read_banner_course <- function(path) {
  purrr::map_df(path, read_banner_course_one)
}

read_banner_course_one <- function(path) {
  if (!file.exists(path)) {
    stop("Can't find that file!")
  }
  # Note that the XLS files exported from BannerWeb are not actually Excel files!
  # They are malformed HTML tables!
  out <- xml2::read_html(path) %>%
    rvest::html_table(fill = TRUE, header = TRUE) %>%
    purrr::pluck(1) %>%
    tibble::repair_names() %>%
    rename_("Student" = ~`Student Name*`) %>%
    filter_(~!is.na(ID)) %>%
    mutate_(Class = ~as.character(Class),
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
#' @export
#' @examples
#' \dontrun{
#'   paths <- head(fs::dir_ls(bannerweb_db))
#'   file_refresh(paths, "banner.myschool.edu")
#' }

file_refresh <- function(path, domain, ...) {
  url <- url_banner_course(path, domain, ...)
  purrr::walk2(url, fs::path(path),
               ~downloader::download(.x, destfile = .y))
}

#' @rdname term_to_banner_term
#' @export
#' @examples
#' \dontrun{
#' bannerweb_db <- "~/Dropbox/SDS/students/enrollments/bannerweb_exports"
#' paths <- head(fs::dir_ls(bannerweb_db))
#' url_banner_course(paths, domain = "myschool.edu")
#' }

url_banner_course <- function(path, domain, ...) {
  filename <- fs::path_file(path)
  parts <- stringr::str_split_fixed(filename, "_", 6)
  crn <- as.numeric(parts[, 2])
  term <- term_to_banner_term(parts[, 5])
  paste0("https://", domain, "/PROD/wroster.P_ExcelList?term=",
         term, "&crn=", crn)
}

#' Read Major/Minor report
#' @param path path to Major/Minor report
#' @return a tibble
#' @import dplyr
#' @export
#' @examples
#' sds <- read_major_report("/tmp/SDS_Student_MajorsMinor_Rpt_05 14 2019.pdf")
#' nrow(sds)

read_major_report <- function(path) {
  raw <- pdftools::pdf_data(path)
  sds <- raw %>%
    purrr::map_df(read_major_report_page)
}

read_major_report_page <- function(x) {
  y <- x %>%
    select(y, x, text) %>%
    tidyr::spread(x, text, sep = "pos_") %>%
    rename(name_last = xpos_36, type = xpos_163, class = xpos_179, email = xpos_594,
           stat = xpos_205,
           major1 = xpos_226, adviser1 = xpos_256,
           major2 = xpos_363, adviser2 = xpos_397,
           minor = xpos_507)

  vars <- names(y)
  y %>%
    filter(!is.na(class)) %>%
    tidyr::unite(name_first, name_last:(matches("type") - 1)) %>%
    tidyr::unite(adviser1, adviser1:(matches("major2") - 1)) %>%
    tidyr::unite(adviser2, adviser2:(matches("minor") - 1)) %>%
    select(-contains("xpos")) %>%
    mutate(name_first = gsub("NA_", "", name_first),
           adviser1 = gsub("NA_", "", adviser1),
           adviser2 = gsub("NA_", "", adviser2),
           is_sds_major = major1 == "SDS" | major2 == "SDS")
}
