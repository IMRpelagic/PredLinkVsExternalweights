Testing out predlink on Blue Whiting
================

## Blue whiting

``` r
library(stockassessment)
load("code/model/bw/wgwide_fit.Rdata")
load("code/model/bw/fitWithPredlink.Rdata")
ssbplot(c("WGWIDE2021" = fit,
          "With predlink" = fit2),
        addCI = T)
```

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
fbarplot(c("WGWIDE2021" = fit,
          "With predlink" = fit2),
        addCI = T)
```

![](README_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

``` r
recplot(c("WGWIDE2021" = fit,
          "With predlink" = fit2),
        addCI = T)
```

![](README_files/figure-gfm/unnamed-chunk-1-3.png)<!-- -->
