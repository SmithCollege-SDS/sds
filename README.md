sds
================

R package providing R Markdown templates for SDS

``` r
# install.packages("devtools")
devtools::install_github("SmithCollege-SDS/sds")
library(sds)

# download class list from BannerWeb
docs <- c("~/Dropbox/git/sds192/student_info/ClassList_10672.xls") %>%
  class_list() %>%
  glimpse()
```

Post any bugs to Issues!
