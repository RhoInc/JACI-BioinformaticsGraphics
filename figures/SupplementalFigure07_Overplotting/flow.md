Scatterplot Flow
================
A Calatroni & J Wildfire
19 May, 2017

-   [set path](#set-path)
-   [packages](#packages)
-   [citation](#citation)
-   [import data](#import-data)
-   [Figure 1](#figure-1)
-   [Figure 2](#figure-2)
-   [Figure 3](#figure-3)
-   [Figure 4](#figure-4)
-   [Figure 5](#figure-5)
-   [Figure 6](#figure-6)
-   [Figure 7](#figure-7)

### set path

``` r
knitr::opts_knit$set(root.dir = '../..')
knitr::opts_chunk$set(warning = FALSE, message = FALSE, comment = NA)
```

### packages

``` r
pacman::p_load(tidyverse, rio)
pacman::p_load(scales)
```

### citation

``` r
citation("ggplot2", auto = FALSE) %>% 
  toBibtex()
```

    @Book{,
      author = {Hadley Wickham},
      title = {ggplot2: Elegant Graphics for Data Analysis},
      publisher = {Springer-Verlag New York},
      year = {2009},
      isbn = {978-0-387-98140-6},
      url = {http://ggplot2.org},
    }

### import data

``` r
df1 <- import("./data/derive/egg_flow_data.rds") 
```

### Figure 1

``` r
ggplot(df1$data[[2]] %>% filter(FITC.A >0, PE.A>0),aes(FITC.A,PE.A)) + 
  geom_point(col='gray50', size=0.7) +
  labs(x="CD63", y="CD203c",
       title = "Raw data") +
  theme_minimal() + 
  theme(aspect.ratio=1)
```

![](flow_files/figure-markdown_github/unnamed-chunk-5-1.png)

### Figure 2

``` r
ggplot(df1$data[[2]] %>% filter(FITC.A >0, PE.A>0),aes(FITC.A,PE.A)) + 
  geom_point(col='gray50', size=0.7) +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x)),
    limits = c(1,1e+05)) +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x)),
    limits = c(1,1e+05)) +
  labs(x="CD63", y="CD203c",
       title = "Transformation: log axis") +
  theme_minimal() + 
  theme(aspect.ratio=1)
```

![](flow_files/figure-markdown_github/unnamed-chunk-6-1.png)

### Figure 3

``` r
ggplot(df1$data[[2]] %>% filter(FITC.A >0, PE.A>0),aes(FITC.A,PE.A)) +  
  geom_point(col='gray50', size=0.7, alpha=1/100) +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x)),
    limits = c(1,1e+05)) +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x)),
    limits = c(1,1e+05)) +
  labs(x="CD63", y="CD203c",
       title = "Transformation: alpha blending") +
  theme_minimal() + 
  theme(aspect.ratio=1)
```

![](flow_files/figure-markdown_github/unnamed-chunk-7-1.png)

### Figure 4

``` r
pacman::p_load(ggExtra)
b <- ggplot(df1$data[[2]] %>% filter(FITC.A >0, PE.A>0),aes(FITC.A,PE.A)) +  
  geom_point(col='gray50', size=0.7, alpha=1/100) +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))) +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))) +
  labs(x="CD63", y="CD203c",
       title = "Annotation:  Marginal Densities") +
  theme_minimal() + 
  theme(aspect.ratio=1)
ggMarginal(b, type = "histogram", fill = "gray70")
```

![](flow_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
ggMarginal(b, type = "density")
```

![](flow_files/figure-markdown_github/unnamed-chunk-8-2.png)

### Figure 5

``` r
ggplot(df1$data[[2]] %>% filter(FITC.A >0, PE.A>0),aes(FITC.A,PE.A)) + 
  geom_point(col='transparent', size=0.01) +
  stat_bin_hex(colour="white", bins=64, na.rm=TRUE) +
  scale_fill_distiller(palette = "Blues", direction=1, guide=FALSE) +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x)),
    limits = c(1,1e+05)) +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x)),
    limits = c(1,1e+05)) +
  labs(x="CD63", y="CD203c",
       title = "Trasformations: Hexagons Binning") +
  theme_minimal() + 
  theme(aspect.ratio=1)
```

![](flow_files/figure-markdown_github/unnamed-chunk-9-1.png)

### Figure 6

``` r
ggplot(df1$data[[2]] %>% filter(FITC.A >0, PE.A>0),aes(FITC.A,PE.A)) +  
  geom_point(col='gray50', size=0.01, alpha=1/100) +
  stat_density2d(aes(fill = ..level..), contour=TRUE, color='gray50', geom="polygon") +
  scale_fill_distiller(palette = "Blues", direction=1, guide=FALSE) + 
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x)),
    limits = c(1,1e+05)) +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x)),
    limits = c(1,1e+05)) +
  labs(x="CD63", y="CD203c",
       title = "Transformation: Contours") +
  theme_minimal() + 
  theme(aspect.ratio=1)
```

![](flow_files/figure-markdown_github/unnamed-chunk-10-1.png)

### Figure 7

``` r
df2 <- df1 %>% unnest(data) %>% filter(FITC.A >0, PE.A>0)
ggplot(df2,aes(FITC.A,PE.A)) +
  geom_point(col='gray50', size=0.01, alpha=1/100) +
  stat_density2d(aes(fill = ..level..), contour=TRUE, color='gray50', geom="polygon") +
  scale_fill_distiller(palette = "Blues", direction=1, guide=FALSE) + 
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x)),
    limits = c(1,1e+05)) +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x)),
    limits = c(1,1e+05)) +
  labs(x="CD63", y="CD203c",
       title = "Expand: Facets") +
  theme_minimal() + 
  theme(aspect.ratio=1)+
  facet_grid(arm_name~planned_visit_name)
```

![](flow_files/figure-markdown_github/unnamed-chunk-11-1.png)
