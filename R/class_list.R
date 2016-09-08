#' Combine class BannerWeb class lists into one
#' @param xls character vector of paths to XLS files exported from BannerWeb
#' @export
#' @importFrom xml2 read_html
#' @importFrom rvest html_table
#' @importFrom magrittr extract2 %>%
#' @importFrom dplyr filter mutate bind_rows
#'
#' @examples
#' \dontrun{
#' docs <- c("~/Dropbox/sds220-f16/student_info/ClassList_20218.xls",
#'           "~/Dropbox/sds220-f16/student_info/ClassList_20902.xls")
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
  xml2::read_html(xls) %>%
    rvest::html_table(fill = TRUE, header = TRUE) %>%
    magrittr::extract2(1) %>%
    dplyr::filter_(~!is.na(ID)) %>%
    dplyr::mutate_(Class = ~as.character(Class))
}
