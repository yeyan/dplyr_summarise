# dplyr summarise bug?





```r
library(tidyverse)
```

```
## Warning: Installed Rcpp (0.12.12) different from Rcpp used to build dplyr (0.12.11).
## Please reinstall dplyr to avoid random crashes or undefined behavior.
```

```r
stats <- read_csv('stats.csv')
```
I am pretty sure that I got the same behaviour before I updated `Rcpp`.


```r
sessionInfo()
```

```
## R version 3.3.2 (2016-10-31)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X El Capitan 10.11.6
## 
## locale:
## [1] en_AU.UTF-8/en_AU.UTF-8/en_AU.UTF-8/C/en_AU.UTF-8/en_AU.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] dplyr_0.7.1     purrr_0.2.2.2   readr_1.1.1     tidyr_0.6.3    
## [5] tibble_1.3.3    ggplot2_2.2.1   tidyverse_1.1.1
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.12     cellranger_1.1.0 plyr_1.8.4       bindr_0.1       
##  [5] forcats_0.2.0    tools_3.3.2      digest_0.6.12    lubridate_1.6.0 
##  [9] jsonlite_1.5     evaluate_0.10.1  nlme_3.1-131     gtable_0.2.0    
## [13] lattice_0.20-35  pkgconfig_2.0.1  rlang_0.1.1      psych_1.7.5     
## [17] yaml_2.1.14      parallel_3.3.2   haven_1.1.0      bindrcpp_0.2    
## [21] xml2_1.1.1       httr_1.2.1       stringr_1.2.0    knitr_1.16      
## [25] hms_0.3          rprojroot_1.2    grid_3.3.2       glue_1.1.1      
## [29] R6_2.2.2         readxl_1.0.0     foreign_0.8-69   rmarkdown_1.6   
## [33] modelr_0.1.0     reshape2_1.4.2   magrittr_1.5     backports_1.1.0 
## [37] scales_0.4.1     htmltools_0.3.6  rvest_0.3.2      assertthat_0.2.0
## [41] mnormt_1.5-5     colorspace_1.3-2 stringi_1.1.5    lazyeval_0.2.0  
## [45] munsell_0.4.3    broom_0.4.2
```

### Use `filter` and `invoke_map` to performance group aggregation


```r
test <- function(impl, size) {
  stats %>%
    filter(message.size==size & implementation==impl) %>%
    select(ts.in, ts.out) %>%
    summarise(begin=min(ts.in),
              end=max(ts.out),
              process.time=end - begin,
              message.rate=size * 10000/as.double(process.time)/1024/1024)
}

invoke_map_df(test, crossing(impl=c('Camel', 'Spark'), size=c(1024, 1024*5, 1024*10)) %>% transpose())
```

```
## # A tibble: 6 x 4
##                 begin                 end process.time message.rate
##                <dttm>              <dttm>       <time>        <dbl>
## 1 2017-07-17 04:27:52 2017-07-17 04:28:13      21 secs    0.4650298
## 2 2017-07-17 04:30:25 2017-07-17 04:32:02      97 secs   30.2029639
## 3 2017-07-17 04:32:58 2017-07-17 04:36:17     199 secs   29.4440955
## 4 2017-07-17 04:18:31 2017-07-17 04:18:54      23 secs    0.4245924
## 5 2017-07-17 04:19:47 2017-07-17 04:21:29     102 secs   28.7224265
## 6 2017-07-17 04:23:10 2017-07-17 04:26:28     198 secs   29.5928030
```

### Using `group_by` and `summarise`


```r
stats %>%
  group_by(implementation, message.size) %>%
  summarise(total.size=sum(message.size), 
            begin=min(ts.in), 
            end=max(ts.out), 
            duration=end-begin,
            message.rate=total.size/as.numeric(duration)/1024/1024) %>%
  ungroup() %>%
  select(begin, end, duration, message.rate)
```

```
## # A tibble: 6 x 4
##                 begin                 end       duration message.rate
##                <dttm>              <dttm>         <time>        <dbl>
## 1 2017-07-17 04:27:52 2017-07-17 04:28:13 21.000000 secs    0.4650298
## 2 2017-07-17 04:30:25 2017-07-17 04:32:02  1.616667 secs   30.2029639
## 3 2017-07-17 04:32:58 2017-07-17 04:36:17  3.316667 secs   29.4440955
## 4 2017-07-17 04:18:31 2017-07-17 04:18:54 23.000000 secs    0.4245924
## 5 2017-07-17 04:19:47 2017-07-17 04:21:29  1.700000 secs   28.7224265
## 6 2017-07-17 04:23:10 2017-07-17 04:26:28  3.300000 secs   29.5928030
```

For some reason the `process.time` is not calculated correctly, but supprisingly `message.rate` which depends on it is correct! Did I do something wrong here?

### Using `group_by` and `do`


```r
stats %>%
  group_by(implementation, message.size) %>%
  do(tibble(total.size=sum(.$message.size), 
            begin=min(.$ts.in), 
            end=max(.$ts.out), 
            duration=end-begin,
            message.rate=total.size/as.numeric(duration)/1024/1024)) %>%
  ungroup() %>%
  select(begin, end, duration, message.rate)
```

```
## # A tibble: 6 x 4
##                 begin                 end duration message.rate
##                <dttm>              <dttm>   <time>        <dbl>
## 1 2017-07-17 04:27:52 2017-07-17 04:28:13  21 secs    0.4650298
## 2 2017-07-17 04:30:25 2017-07-17 04:32:02  97 secs   30.2029639
## 3 2017-07-17 04:32:58 2017-07-17 04:36:17 199 secs   29.4440955
## 4 2017-07-17 04:18:31 2017-07-17 04:18:54  23 secs    0.4245924
## 5 2017-07-17 04:19:47 2017-07-17 04:21:29 102 secs   28.7224265
## 6 2017-07-17 04:23:10 2017-07-17 04:26:28 198 secs   29.5928030
```

Do's behaviour matches `filter` and `invoke_map` combination.
