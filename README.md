
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggNetView <img src="man/figures/logo.png" align="right" width="240" hight="240" />

<!-- badges: start -->

<!-- badges: end -->

ggNetView is an R package for network analysis and visualization. It
provides flexible and publication-ready tools for exploring complex
biological and ecological networks.

</br> </br> </br>

## Installation

You can install the development version of ggNetView from
[GitHub](https://github.com/) with:

    # install.packages("devtools")
    devtools::install_github("Jiawang1209/ggNetView")

## Example1

### Step1: load ggNetView

``` r
library(ggNetView)
library(ggplot2)
library(ggnewscale)
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
#>    name     modularity modularity2 modularity3 Modularity Degree Strength Kingdom Phylum Class Order
#>    <chr>    <fct>      <ord>       <chr>       <ord>       <dbl>    <dbl> <chr>   <chr>  <chr> <chr>
#>  1 ASV_916  1          1           1           1              58     50.5 Bacter… Prote… Gamm… Unas…
#>  2 ASV_777  1          1           1           1              58     48.7 Bacter… Acido… Acid… Unas…
#>  3 ASV_606  1          1           1           1              55     45.8 Bacter… Actin… Acti… Acti…
#>  4 ASV_740  1          1           1           1              54     47.2 Bacter… Acido… Acid… Unas…
#>  5 ASV_1449 1          1           1           1              54     44.5 Bacter… Actin… Acti… Unas…
#>  6 ASV_2362 1          1           1           1              54     47.4 Bacter… Prote… Alph… Rhiz…
#>  7 ASV_1572 1          1           1           1              52     45.3 Bacter… Acido… Acid… Unas…
#>  8 ASV_2433 1          1           1           1              52     43.0 Bacter… Actin… Acti… Acti…
#>  9 ASV_1992 1          1           1           1              52     43.0 Bacter… Unass… Unas… Unas…
#> 10 ASV_568  1          1           1           1              51     45.1 Bacter… Unass… Unas… Unas…
#> # ℹ 2,039 more rows
#> # ℹ 3 more variables: Family <chr>, Genus <chr>, Species <chr>
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
  layout.module = "random",
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
#> Warning: No shared levels found between `names(values)` of the manual scale and the data's fill
#> values.
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
  layout.module = "random",
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
#> Warning: No shared levels found between `names(values)` of the manual scale and the data's fill
#> values.
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
  layout.module = "random",
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
#> Warning: No shared levels found between `names(values)` of the manual scale and the data's fill
#> values.
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
  layout.module = "random",
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
#> Warning: No shared levels found between `names(values)` of the manual scale and the data's fill
#> values.
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
#> Running under: macOS Sequoia 15.6
#> 
#> Matrix products: default
#> BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
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
#> [1] ggnewscale_0.5.2 ggplot2_4.0.2    ggNetView_1.4.14
#> 
#> loaded via a namespace (and not attached):
#>   [1] mnormt_2.1.2          gridExtra_2.3         httr2_1.2.1           rlang_1.1.7          
#>   [5] magrittr_2.0.4        otel_0.2.0            matrixStats_1.5.0     compiler_4.5.1       
#>   [9] vctrs_0.7.1           stringr_1.6.0         pkgconfig_2.0.3       fastmap_1.2.0        
#>  [13] backports_1.5.0       labeling_0.4.3        ggraph_2.2.2          utf8_1.2.6           
#>  [17] rmarkdown_2.30        preprocessCore_1.70.0 purrr_1.2.1           xfun_0.56            
#>  [21] gert_2.1.5            cachem_1.1.0          jsonlite_2.0.0        tweenr_2.0.3         
#>  [25] psych_2.6.1           parallel_4.5.1        cluster_2.1.8.1       R6_2.6.1             
#>  [29] stringi_1.8.7         RColorBrewer_1.1-3    rpart_4.1.24          Rcpp_1.1.1           
#>  [33] iterators_1.0.14      knitr_1.51            usethis_3.2.1         WGCNA_1.74           
#>  [37] base64enc_0.1-6       gitcreds_0.1.2        connectcreds_0.1.0    FNN_1.1.4.1          
#>  [41] Matrix_1.7-4          splines_4.5.1         nnet_7.3-20           igraph_2.2.1         
#>  [45] tidyselect_1.2.1      rstudioapi_0.18.0     dichromat_2.0-0.1     yaml_2.3.12          
#>  [49] viridis_0.6.5         doParallel_1.0.17     codetools_0.2-20      curl_7.0.0           
#>  [53] lattice_0.22-7        tibble_3.3.1          Biobase_2.70.0        withr_3.0.2          
#>  [57] S7_0.2.1              askpass_1.2.1         evaluate_1.0.5        foreign_0.8-90       
#>  [61] desc_1.4.3            survival_3.8-3        polyclip_1.10-7       pillar_1.11.1        
#>  [65] rsconnect_1.6.1       whisker_0.4.1         checkmate_2.3.4       foreach_1.5.2        
#>  [69] stats4_4.5.1          generics_0.1.4        rprojroot_2.1.1       credentials_2.0.3    
#>  [73] scales_1.4.0          glue_1.8.0            Hmisc_5.2-5           tools_4.5.1          
#>  [77] sys_3.4.3             data.table_1.18.2.1   fs_1.6.6              fastcluster_1.3.0    
#>  [81] graphlayouts_1.2.2    tidygraph_1.3.1       grid_4.5.1            impute_1.82.0        
#>  [85] tidyr_1.3.2           gh_1.5.0              colorspace_2.1-2      nlme_3.1-168         
#>  [89] ggforce_0.5.0         htmlTable_2.4.3       Formula_1.2-5         cli_3.6.5            
#>  [93] rappdirs_0.3.4        viridisLite_0.4.3     dplyr_1.2.0           gtable_0.3.6         
#>  [97] dynamicTreeCut_1.63-1 digest_0.6.39         BiocGenerics_0.56.0   ggrepel_0.9.6        
#> [101] htmlwidgets_1.6.4     farver_2.1.2          memoise_2.0.1         htmltools_0.5.9      
#> [105] multtest_2.64.0       lifecycle_1.0.5       openssl_2.3.4         MASS_7.3-65
```

#### Citation

If you use ggNetView in your research, please cite:

    Yue Liu (2025). ggNetView: An R package for complex biological and ecological network analysis and visualization. R package version 0.1.0. 
    https://github.com/Jiawang1209/ggNetView

<h4 align="center">

©微信公众号 RPython
</h5>
