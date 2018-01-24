sds
================

R package providing R Markdown templates for SDS

Install
-------

``` r
# install.packages("devtools")
devtools::install_github("SmithCollege-SDS/sds")
```

Load
----

``` r
library(sds)
```

Get class list
--------------

``` r
# download class list from BannerWeb
docs <- c("~/Dropbox/git/sds192/student_info/ClassList_10672.xls") %>%
  class_list() %>%
  glimpse()
```

Print hex logo!
---------------

``` r
hex_logo(file = "inst/sds_hex.png")
```

![SDS hex logo](inst/sds_hex.png)

Write a letter on official SDS letterhead
-----------------------------------------

Select **File -&gt; New File -&gt; R Markdown...** and choose the **Smith SDS Letter** template

Post any bugs to Issues!
