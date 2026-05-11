
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggNetView <img src="man/figures/logo.png" align="right" width="240" hight="240" />

<!-- badges: start -->

<!-- badges: end -->

ggNetView is an R package for network analysis and visualization. It
provides flexible and publication-ready tools for exploring complex
biological and ecological networks.

</br> </br> </br>

## Installation

`ggNetView` depends on a number of CRAN packages. We recommend
installing the required dependencies first, and then installing
`ggNetView` from GitHub.

### Step1: install CRAN dependencies

    cran_pkgs <- c(
      "boot", "dplyr", "FNN", "future", "future.apply",
      "ggforce", "ggnewscale", "ggplot2", "ggraph", "ggrepel",
      "Hmisc", "huge", "igraph", "magrittr", "MASS",
      "Matrix", "patchwork", "progressr", "psych", "purrr",
      "qgraph", "Rcpp", "RcppArmadillo", "readr", "rlang",
      "scales", "scatterpie", "stringr", "tibble", "tidygraph",
      "tidyr", "vegan", "VGAM", "WGCNA"
    )

    new_pkgs <- cran_pkgs[!cran_pkgs %in% installed.packages()[, "Package"]]
    if (length(new_pkgs)) install.packages(new_pkgs)

### Step2: (optional) install suggested packages

These packages are not required for the core functionality, but enable
additional features (e.g. dynamic tree cut, node influence, vignettes,
tests):

    install.packages(c("dynamicTreeCut", "influential",
                       "knitr", "rmarkdown",
                       "RobustRankAggreg", "testthat"))

### Step3: install ggNetView from GitHub

    # install.packages("devtools")
    devtools::install_github("Jiawang1209/ggNetView")

## Example1

### Step1: load ggNetView

``` r
library(ggplot2)
#> Warning: package 'ggplot2' was built under R version 4.5.2
library(ggnewscale)
library(ggNetView)
#> 
#>                                                ░██               ░██
#>                                                ░██
#>  ░████████  ░████████ ░████████   ░███████  ░████████ ░██    ░██ ░██ ░███████  ░██    ░██    ░██
#> ░██    ░██ ░██    ░██ ░██    ░██ ░██    ░██    ░██    ░██    ░██ ░██░██    ░██ ░██    ░██    ░██
#> ░██    ░██ ░██    ░██ ░██    ░██ ░█████████    ░██     ░██  ░██  ░██░█████████  ░██  ░████  ░██
#> ░██   ░███ ░██   ░███ ░██    ░██ ░██           ░██      ░██░██   ░██░██          ░██░██ ░██░██
#>  ░█████░██  ░█████░██ ░██    ░██  ░███████      ░████    ░███    ░██ ░███████     ░███   ░███
#>        ░██        ░██
#>  ░███████   ░███████
#> 
#> 
#> 
#> ggNetView: Reproducible and Deterministic Network Analysis and Visualization
#> Version: 0.1.0
#> 
#>   Authors:     Yue Liu, Chao Wang
#>   Maintainer:  Yue Liu <yueliu@iae.ac.cn>
#> 
#>   Manual:      https://jiawang1209.github.io/ggNetView-manual/
#>   GitHub:      https://github.com/Jiawang1209/ggNetView
#>   Bug Reports: https://github.com/Jiawang1209/ggNetView/issues
#> 
#>   Type citation('ggNetView') for how to cite this package.
```

### Step2: load Data

> You can load raw matrix

``` r
data("otu_tab")

otu_tab[1:5, 1:5]
#>        KO1  KO2  KO3  KO4  KO5
#> ASV_1 1113 1968  816 1372 1062
#> ASV_2 1922 1227 2355 2218 2885
#> ASV_3  568  460  899  902 1226
#> ASV_4 1433  400  535  759 1287
#> ASV_6  882  673  819  888 1475
```

> You can load rarely matrix. Note : the rownames of `otu_rare` is the
> features.

``` r
data("otu_rare")

otu_tab[1:5, 1:5]
#>        KO1  KO2  KO3  KO4  KO5
#> ASV_1 1113 1968  816 1372 1062
#> ASV_2 1922 1227 2355 2218 2885
#> ASV_3  568  460  899  902 1226
#> ASV_4 1433  400  535  759 1287
#> ASV_6  882  673  819  888 1475
```

> 

``` r
data("otu_rare_relative")

otu_rare_relative[1:5, 1:5]
#>              KO1        KO2        KO3        KO4        KO5
#> ASV_1 0.03306667 0.05453333 0.02013333 0.03613333 0.02686667
#> ASV_2 0.05750000 0.03393333 0.06046667 0.05810000 0.07320000
#> ASV_3 0.01733333 0.01296667 0.02290000 0.02336667 0.03106667
#> ASV_4 0.04266667 0.01093333 0.01416667 0.01933333 0.03346667
#> ASV_6 0.02646667 0.01856667 0.02110000 0.02353333 0.03806667
```

> You can load node annotation. Note : the rownames of `tax_tab` is
> NULL.

``` r
data("tax_tab")

tax_tab[1:5, 1:5]
#> # A tibble: 5 × 5
#>   OTUID  Kingdom  Phylum          Class          Order            
#>   <chr>  <chr>    <chr>           <chr>          <chr>            
#> 1 ASV_2  Archaea  Thaumarchaeota  Unassigned     Nitrososphaerales
#> 2 ASV_3  Bacteria Verrucomicrobia Spartobacteria Unassigned       
#> 3 ASV_31 Bacteria Actinobacteria  Actinobacteria Actinomycetales  
#> 4 ASV_27 Archaea  Thaumarchaeota  Unassigned     Nitrososphaerales
#> 5 ASV_9  Bacteria Unassigned      Unassigned     Unassigned
```

### Step3: create graph object

``` r
obj <- build_graph_from_mat(
  mat = otu_rare_relative,
  transfrom.method = "none",
  method = "WGCNA",
  cor.method = "pearson",
  proc = "BH",
  r.threshold = 0.7,
  p.threshold = 0.05,
  node_annotation = tax_tab
)

obj
#> # A tbl_graph: 2049 nodes and 9602 edges
#> #
#> # An undirected simple graph with 100 components
#> #
#> # Node Data: 2,049 × 14 (active)
#>    name    modularity modularity2 modularity3 Modularity Degree Strength Kingdom
#>    <chr>   <fct>      <ord>       <chr>       <ord>       <dbl>    <dbl> <chr>  
#>  1 ASV_916 1          1           1           1              58     50.5 Bacter…
#>  2 ASV_777 1          1           1           1              58     48.7 Bacter…
#>  3 ASV_606 1          1           1           1              55     45.8 Bacter…
#>  4 ASV_740 1          1           1           1              54     47.2 Bacter…
#>  5 ASV_14… 1          1           1           1              54     44.5 Bacter…
#>  6 ASV_23… 1          1           1           1              54     47.4 Bacter…
#>  7 ASV_15… 1          1           1           1              52     45.3 Bacter…
#>  8 ASV_24… 1          1           1           1              52     43.0 Bacter…
#>  9 ASV_19… 1          1           1           1              52     43.0 Bacter…
#> 10 ASV_568 1          1           1           1              51     45.1 Bacter…
#> # ℹ 2,039 more rows
#> # ℹ 6 more variables: Phylum <chr>, Class <chr>, Order <chr>, Family <chr>,
#> #   Genus <chr>, Species <chr>
#> #
#> # Edge Data: 9,602 × 5
#>    from    to weight correlation corr_direction
#>   <int> <int>  <dbl>       <dbl> <chr>         
#> 1  1771  1825  0.793       0.793 Positive      
#> 2   594   597  0.895       0.895 Positive      
#> 3   588   597  0.864       0.864 Positive      
#> # ℹ 9,599 more rows
```

### Step4: ggNetView to plot

> Basic network plot

``` r
p1 <- ggNetView(
  graph_obj = obj,
  layout = "gephi",
  layout.module = "adjacent",
  group.by = "Modularity",
  fill.by = "Modularity",
  pointsize = c(1, 5),
  center = F,
  jitter = F,
  mapping_line = F,
  shrink = 0.9,
  linealpha = 0.2,
  linecolor = "#d9d9d9"
) 

p1
```

<img src="man/figures/README-unnamed-chunk-8-1.png" alt="" width="100%" style="display: block; margin: auto;" />

    ggsave(file = "Output/p1.pdf",
           plot = p1,
           height = 10,
           width = 10)

> Add outer line in netwotk plot

``` r
p2 <- ggNetView(
  graph_obj = obj,
  layout = "gephi",
  layout.module = "adjacent",
  group.by = "Modularity",
  fill.by = "Modularity",
  pointsize = c(1, 5),
  center = F,
  jitter = TRUE,
  jitter_sd = 0.15,
  mapping_line = TRUE,
  shrink = 0.9,
  linealpha = 0.2,
  linecolor = "#d9d9d9",
  add_outer = T,
  label = T
) 
#> Coordinate system already present.
#> ℹ Adding new coordinate system, which will replace the existing one.

p2
```

<img src="man/figures/README-unnamed-chunk-9-1.png" alt="" width="100%" style="display: block; margin: auto;" />

    ggsave(file = "Output/p2.pdf",
           plot = p2,
           height = 10,
           width = 10)

> Change the fill of node points.

``` r
p3 <- ggNetView(
  graph_obj = obj,
  layout = "gephi",
  layout.module = "adjacent",
  group.by = "Modularity",
  fill.by = "Phylum",
  pointsize = c(1, 5),
  center = F,
  jitter = TRUE,
  jitter_sd = 0.15,
  mapping_line = TRUE,
  shrink = 0.9,
  linealpha = 0.2,
  linecolor = "#d9d9d9",
  add_outer = T,
  label = T
) 
#> Coordinate system already present.
#> ℹ Adding new coordinate system, which will replace the existing one.

p3
```

<img src="man/figures/README-unnamed-chunk-10-1.png" alt="" width="100%" style="display: block; margin: auto;" />

    ggsave(file = "Output/p3.pdf",
           plot = p3,
           height = 10,
           width = 10)

> Change the color of node points.

``` r
p4 <- ggNetView(
  graph_obj = obj,
  layout = "gephi",
  layout.module = "adjacent",
  group.by = "Modularity",
  fill.by = "Phylum",
  color.by = "Phylum",
  pointsize = c(1, 5),
  center = F,
  jitter = TRUE,
  jitter_sd = 0.15,
  mapping_line = TRUE,
  shrink = 0.9,
  linealpha = 0.2,
  linecolor = "#d9d9d9",
  add_outer = T,
  label = T
) 
#> Coordinate system already present.
#> ℹ Adding new coordinate system, which will replace the existing one.

p4
```

<img src="man/figures/README-unnamed-chunk-11-1.png" alt="" width="100%" style="display: block; margin: auto;" />

    ggsave(file = "Output/p4.pdf",
           plot = p4,
           height = 10,
           width = 10)

> Add node label

``` r
p5 <- ggNetView(
  graph_obj = obj,
  layout = "gephi",
  layout.module = "adjacent",
  group.by = "Modularity",
  fill.by = "Modularity",
  pointsize = c(1, 5),
  center = F,
  jitter = TRUE,
  jitter_sd = 0.15,
  mapping_line = TRUE,
  shrink = 0.9,
  linealpha = 0.2,
  linecolor = "#d9d9d9",
  add_outer = T,
  label = T,
  pointlabel = "top1"
) 
#> Coordinate system already present.
#> ℹ Adding new coordinate system, which will replace the existing one.

p5
```

<img src="man/figures/README-unnamed-chunk-12-1.png" alt="" width="100%" style="display: block; margin: auto;" />

    ggsave(file = "Output/p3.pdf", 
           plot = p5,
           height = 10,
           width = 10)

## Example2

> Get information of graph_object

``` r
Sub_module_1 <- get_subgraph(graph_obj = obj, select_module = "1")
#>    Module Number
#> 1       1    416
#> 2       7    161
#> 3       6    137
#> 4       9    121
#> 5       4    112
#> 6       2    105
#> 7       3    104
#> 8      11    101
#> 9       8     87
#> 10     10     80
#> 11      5     78
#> 12     13     70
#> 13     16     52
#> 14     15     51
#> 15     14     46
#> 16 Others    328

names(Sub_module_1)
#> [1] "sub_graph_all"    "stat_module"      "sub_graph_select"
```

## Example3

``` r
# load test data in ggNetView
data("Envdf_4st")
data("Spedf")
```

``` r
out1 <- gglink_heatmaps(
  env = Envdf_4st,
  spec = Spedf,
  env_select = list(Env01 = 1:14, 
                    Env02 = 15:28,
                    Env03 = 29:42,
                    Env04 = 43:56),
  spec_select = list(Spec01 = 1:8),
  relation_method = "correlation",
  spec_layout = "circle_outline",
  cor.method = "pearson",
  cor.use = "pairwise",
  r = 6,
  distance = 1,
  orientation = c("top_right", "bottom_right", "top_left", "bottom_left")
)
#> The max module in network is 2 we use the 2  modules for next analysis

out1[[1]]
```

<img src="man/figures/README-unnamed-chunk-15-1.png" alt="" width="100%" style="display: block; margin: auto;" />

## Example4

> Leave lines with a significance level less than 0.05, and change the
> color of heatmap.

``` r
out2 <- gglink_heatmaps(
  env = Envdf_4st,
  spec = Spedf,
  env_select = list(Env01 = 1:14,
                    Env03 = 29:40,
                    Env04 = 43:50),
  spec_select = list(Spec01 = 1:8),
  relation_method = "correlation",
  spec_layout = "circle_outline",
  cor.method = "pearson",
  cor.use = "pairwise",
  drop_nonsig = TRUE,
  HeatmapColorBar = list(c("#2166ac", "#b2182b"), 
                         c("#1b7837", "#762a83"),
                         c("#4393c3", "#d6604d")),
  HeatmapPointFill = "#8c6bb1",
  CorePointFill = "#225ea8",
  HeatmapLabelOrient = 45,
  r = 6,
  distance = 1,
  orientation = c("top_right", "top_left", "bottom_left")
)
#> The max module in network is 2 we use the 2  modules for next analysis

out2[[2]]
```

<img src="man/figures/README-unnamed-chunk-16-1.png" alt="" width="100%" style="display: block; margin: auto;" />

## sessionInfo

``` r
sessionInfo()
#> R version 4.5.1 (2025-06-13)
#> Platform: aarch64-apple-darwin20
#> Running under: macOS Tahoe 26.3.1
#> 
#> Matrix products: default
#> BLAS:   /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/lib/libRblas.0.dylib 
#> LAPACK: /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.1
#> 
#> locale:
#> [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
#> 
#> time zone: Asia/Shanghai
#> tzcode source: internal
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] ggNetView_0.1.0  ggnewscale_0.5.2 ggplot2_4.0.3   
#> 
#> loaded via a namespace (and not attached):
#>  [1] gtable_0.3.6          impute_1.82.0         xfun_0.57            
#>  [4] htmlwidgets_1.6.4     psych_2.6.3           ggrepel_0.9.8        
#>  [7] lattice_0.22-7        vctrs_0.7.3           tools_4.5.1          
#> [10] generics_0.1.4        parallel_4.5.1        tibble_3.3.1         
#> [13] cluster_2.1.8.1       pkgconfig_2.0.3       Matrix_1.7-4         
#> [16] data.table_1.18.4     checkmate_2.3.4       RColorBrewer_1.1-3   
#> [19] S7_0.2.2              lifecycle_1.0.5       FNN_1.1.4.1          
#> [22] compiler_4.5.1        farver_2.1.2          stringr_1.6.0        
#> [25] mnormt_2.1.2          ggforce_0.5.0         graphlayouts_1.2.3   
#> [28] codetools_0.2-20      htmltools_0.5.9       yaml_2.3.12          
#> [31] htmlTable_2.5.0       preprocessCore_1.70.0 Formula_1.2-5        
#> [34] pillar_1.11.1         tidyr_1.3.2           MASS_7.3-65          
#> [37] cachem_1.1.0          viridis_0.6.5         Hmisc_5.2-5          
#> [40] iterators_1.0.14      rpart_4.1.24          foreach_1.5.2        
#> [43] nlme_3.1-168          tidyselect_1.2.1      digest_0.6.39        
#> [46] stringi_1.8.7         purrr_1.2.2           dplyr_1.2.1          
#> [49] labeling_0.4.3        splines_4.5.1         polyclip_1.10-7      
#> [52] fastmap_1.2.0         grid_4.5.1            colorspace_2.1-2     
#> [55] cli_3.6.6             magrittr_2.0.5        base64enc_0.1-6      
#> [58] ggraph_2.2.2          dichromat_2.0-0.1     tidygraph_1.3.1      
#> [61] survival_3.8-3        dynamicTreeCut_1.63-1 utf8_1.2.6           
#> [64] foreign_0.8-90        withr_3.0.2           scales_1.4.0         
#> [67] backports_1.5.1       rmarkdown_2.31        matrixStats_1.5.0    
#> [70] igraph_2.3.1          otel_0.2.0            nnet_7.3-20          
#> [73] gridExtra_2.3         fastcluster_1.3.0     memoise_2.0.1        
#> [76] evaluate_1.0.5        knitr_1.51            doParallel_1.0.17    
#> [79] viridisLite_0.4.3     rlang_1.2.0           Rcpp_1.1.1-1.1       
#> [82] glue_1.8.1            tweenr_2.0.3          rstudioapi_0.18.0    
#> [85] WGCNA_1.74            R6_2.6.1
```

#### Citation

If you use ggNetView in your research, please cite:

    Yue Liu, Chao Wang (2026). ggNetView: An R Package for Reproducible and Deterministic Network Analysis and Visualization. 

#### Github & Manual

    https://github.com/Jiawang1209/ggNetView
    https://jiawang1209.github.io/ggNetView-manual/

<h4 align="center">

©微信公众号 RPython
</h5>
