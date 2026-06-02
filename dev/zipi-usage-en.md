# `ggnetview_zipi()` Usage Guide

## What this function does

`ggnetview_zipi()` computes two metrics for every node in a modular network and uses them to classify nodes into four topological roles (Guimerà & Amaral, 2005):

- **Zi (within-module connectivity)**: how strongly a node connects within its own module. High Zi = the node is a core member inside its module.
- **Pi (among-module connectivity / participation coefficient)**: how much a node connects across modules. High Pi = the node acts as a bridge linking several modules.

Using the default thresholds `Zi = 2.5` and `Pi = 0.62`, nodes are split into four quadrants:

| Role | Zi | Pi | Meaning |
|------|----|----|---------|
| **Peripherals** | low | low | Few connections within and across modules; edge nodes |
| **Module hubs** | high | low | Core within their module, but weakly connected across modules |
| **Connectors** | low | high | Not prominent within their module, but strongly bridge across modules |
| **Network hubs** | high | high | Core both within and across modules; most critical to the whole network |

## Function signature

```r
ggnetview_zipi(
  nodes_bulk,            # node table (data.frame / tibble)
  z_bulk_mat,            # adjacency or correlation matrix
  modularity_col,        # name of the column holding module labels
  degree_col,            # name of the column holding node degree
  zi_threshold = 2.5,    # Zi threshold
  pi_threshold = 0.62,   # Pi threshold
  na.rm = FALSE          # drop rows whose Zi/Pi is NA
)
```

### Input requirements (common pitfalls)

- **`nodes_bulk`**: node IDs must live in `rownames`, **or** in a column named `name` (the default output of `tidygraph::as_tibble()` already qualifies). It must contain both `modularity_col` and `degree_col`, and **neither column may contain NA**.
- **`z_bulk_mat`**: must have `rownames` and `colnames`, and the node IDs in `nodes_bulk` must be a subset of the matrix row names. Non-zero entries are treated as edges; `NA`/`Inf` are automatically replaced with 0.
- The two inputs are aligned by ID automatically — you don't need to sort them yourself.

### Return value

A **list** with two elements:

- **`$data`**: the original `nodes_bulk` plus three new columns — `within_module_connectivities` (Zi), `among_module_connectivities` (Pi), and `type` (the role).
- **`$plot`**: a ggplot object — the Zi-Pi four-quadrant scatter plot (with shaded background regions and quadrant labels).

## Minimal end-to-end example

Run the whole pipeline on the package's bundled example data:

```r
library(ggNetView)

# 1. Prepare data and build the graph
data(otu_rare_relative)
data(tax_tab)

mat <- as.matrix(otu_rare_relative)
mat <- mat[order(rowSums(mat), decreasing = TRUE)[seq_len(50)], ]  # 50 most abundant OTUs

obj <- build_graph_from_mat(
  mat           = mat,
  method        = "cor",
  cor.method    = "spearman",
  proc          = "BH",
  r.threshold   = 0.5,
  p.threshold   = 0.05,
  module.method = "Fast_greedy",
  seed          = 1
)
# The resulting graph's node table already carries Degree, Strength, and Modularity

# 2. Extract the two inputs Zi-Pi needs
nodes_bulk <- get_graph_nodes(obj)        # node table
adj_mat    <- get_graph_adjacency(obj)    # adjacency matrix

# 3. Compute Zi-Pi and classify
zp <- ggnetview_zipi(
  nodes_bulk     = nodes_bulk,
  z_bulk_mat     = adj_mat,
  modularity_col = "Modularity",
  degree_col     = "Degree"
)

# 4. Inspect the results
zp$plot                  # four-quadrant plot
head(zp$data)            # node table with Zi, Pi, type
```

## Common usage

### List the nodes in each role

```r
# All Network hubs (the most critical nodes)
subset(zp$data, type == "Network hubs", select = c(name, type,
       within_module_connectivities, among_module_connectivities))

# Count nodes per role
table(zp$data$type, useNA = "ifany")
```

### Adjust the thresholds

Different fields define hubs differently, so you can re-classify with custom thresholds:

```r
zp2 <- ggnetview_zipi(
  nodes_bulk, adj_mat, "Modularity", "Degree",
  zi_threshold = 2.0,   # relax the Zi threshold
  pi_threshold = 0.60
)
```

### Drop unclassifiable nodes

Single-member modules (a module containing only one node) have an undefined Zi, so their `type` is `NA` by default. To remove them directly:

```r
zp3 <- ggnetview_zipi(nodes_bulk, adj_mat, "Modularity", "Degree",
                      na.rm = TRUE)
```

### Customize the plot

`$plot` is a standard ggplot object, so you can layer on top of it:

```r
library(ggplot2)
zp$plot +
  labs(title = "Zi-Pi roles of my network") +
  theme(legend.position = "bottom")

# Save
ggsave("zipi_plot.pdf", zp$plot, width = 6, height = 6)
```

## Combining with IVI / centrality

Zi-Pi gives you the *topological role* (what role a node plays), while IVI / centrality give you an *influence ranking* (how important a node is) — the two are complementary. A recommended workflow: first compute influence with `get_node_centrality()` and `get_node_ivi()`, then look at roles with `ggnetview_zipi()`. A node that ranks high on IVI **and** lands in the **Network hubs** quadrant is the most defensible candidate for downstream biological follow-up.

## Reference

Guimerà R, Amaral LAN (2005). "Functional cartography of complex metabolic networks." *Nature* 433(7028):895-900.
