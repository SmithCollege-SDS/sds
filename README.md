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
library(tidyverse)
# download class list from BannerWeb
docs <- c("~/Dropbox/git/sds192/student_info/ClassList_10672.xls") %>%
  class_list() %>%
  glimpse()
```

Add Smith theming to a `ggplot`
-------------------------------

``` r
library(ggplot2)
ggplot(mpg, aes(y = hwy, x = reorder(model, hwy), fill = hwy)) +
  geom_col(position = "dodge") +
  coord_flip() +
  facet_wrap(~year) +
  scale_fill_smith_cd(midpoint = 20)
```

![](README_files/figure-markdown_github/unnamed-chunk-4-1.png)

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
