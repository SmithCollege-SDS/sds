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
