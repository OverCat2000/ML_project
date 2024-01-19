R
================

``` r
library(ggplot2)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(GGally)
```

    ## Warning: package 'GGally' was built under R version 4.3.2

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

``` r
library(readr)
Medicalpremium <- read_csv("C:/Users/malis/3rdYear/New folder (3)/New folder/Medicalpremium.csv")
```

    ## Rows: 986 Columns: 11

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (11): Age, Diabetes, BloodPressureProblems, AnyTransplants, AnyChronicDi...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
df = Medicalpremium

lapply(df, class)
```

    ## $Age
    ## [1] "numeric"
    ## 
    ## $Diabetes
    ## [1] "numeric"
    ## 
    ## $BloodPressureProblems
    ## [1] "numeric"
    ## 
    ## $AnyTransplants
    ## [1] "numeric"
    ## 
    ## $AnyChronicDiseases
    ## [1] "numeric"
    ## 
    ## $Height
    ## [1] "numeric"
    ## 
    ## $Weight
    ## [1] "numeric"
    ## 
    ## $KnownAllergies
    ## [1] "numeric"
    ## 
    ## $HistoryOfCancerInFamily
    ## [1] "numeric"
    ## 
    ## $NumberOfMajorSurgeries
    ## [1] "numeric"
    ## 
    ## $PremiumPrice
    ## [1] "numeric"

``` r
unique(df$NumberOfMajorSurgeries)
```

    ## [1] 0 1 2 3

``` r
ggplot() +
  geom_boxplot(df, mapping=aes(x=Age, fill=as.factor(NumberOfMajorSurgeries)))
```

![](insurance_data_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
ggplot() +
  geom_boxplot(df, mapping=aes(x=Weight, fill=as.factor(Diabetes)))
```

![](insurance_data_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
ggplot() +
  geom_point(df, mapping=aes(x=df$Weight, y=df$Height, color=as.factor(df$BloodPressureProblems)))
```

![](insurance_data_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
ggcorr(df[c("Age", "Weight", "Height", "PremiumPrice")])
```

![](insurance_data_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
df[c("Age", "Weight", "Height")]
```

    ## # A tibble: 986 × 3
    ##      Age Weight Height
    ##    <dbl>  <dbl>  <dbl>
    ##  1    45     57    155
    ##  2    60     73    180
    ##  3    36     59    158
    ##  4    52     93    183
    ##  5    38     88    166
    ##  6    30     69    160
    ##  7    33     54    150
    ##  8    23     79    181
    ##  9    48     74    169
    ## 10    38     93    182
    ## # ℹ 976 more rows
