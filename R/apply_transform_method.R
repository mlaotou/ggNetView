apply_transform_method <- function(mat, transfrom.method) {
  rarefied <- NULL

  switch(
    transfrom.method,
    none = mat,
    scale = t(scale(t(mat), scale = TRUE, center = TRUE)),
    center = t(scale(t(mat), scale = FALSE, center = TRUE)),
    log2 = log2(mat + 1),
    log10 = log10(mat + 1),
    ln = log(mat + 1),
    rrarefy = {
      rarefied <- t(vegan::rrarefy(t(mat), min(colSums(mat))))
      rarefied
    },
    rrarefy_relative = {
      rarefied <- t(vegan::rrarefy(t(mat), min(colSums(mat))))
      sweep(rarefied, 2, colSums(rarefied), "/", check.margin = FALSE)
    }
  )
}
