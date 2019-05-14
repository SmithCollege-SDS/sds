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
