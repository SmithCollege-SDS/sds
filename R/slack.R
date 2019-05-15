#' Unzips slack message exports.
#'
#' Removes previously extracted `data` directory if exists
#' and extracts into `data` directory.
#'
#' @importFrom jsonlite fromJSON
#' @import dplyr
#' @param dir directory
#' @param ... currently ignored
#  unzip_slack_export <- function(group_name, date){
#  filename <- sprintf("%s Slack export %s", group_name, date)
#  unzip_cmd <- sprintf("rm -rf data && mkdir -p data && unzip '%s.zip' -d data", filename)
#  system(unzip_cmd)
#}

# unzip the extract
# group_name <- 'elise alex'
# date <- 'Sep 2 2017'
# unzip_slack_export(group_name, date)
#
# # explore
# channels_df <- jsonlite::fromJSON('data/channels.json', flatten = T)
# users_df <- jsonlite::fromJSON('data/users.json', flatten = T)
#' @examples
#' \dontrun{
#' slack_labs("~/Dropbox/git/sds192/student_info/SDS 192-f17 Slack export Sep 24 2017/")
#' }

slack_labs <- function(dir, ...) {
  users_df <- jsonlite::fromJSON(file.path(dir, "users.json"), flatten = T) %>%
    dplyr::select(id, real_name, profile.email)
  labs <- list.files(dir, pattern = "lab*")
  lab_days <- list.files(file.path(dir, labs[1]), full.names = TRUE)
  labs_df <- lapply(lab_days, jsonlite::fromJSON, flatten = TRUE) %>%
    bind_rows() %>%
    filter(!subtype %in% c("channel_join", "file_share"),
           !user == "U6X0GDF8E") %>%
    left_join(users_df, by = c("user" = "id"))
  lapply(labs_df$text, nchar) %>% unlist()
}
