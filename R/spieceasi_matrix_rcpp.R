# SPIEC-EASI Rcpp implementation
# Uses spieceasi_norm_cpp for fast CLR; rest mirrors spieceasi_matrix.R

#' @useDynLib ggNetView, .registration = TRUE

# Local helpers (mirror spieceasi_matrix.R)
sparseiCov_local <- function(data, method, npn = FALSE, verbose = FALSE, cov.output = TRUE, ...) {
  if (npn) {
    data <- huge::huge.npn(data, verbose = verbose)
  }
  args <- list(...)
  method <- switch(method, glasso = "glasso", mb = "mb", stop("method not supported"))
  if (is.null(args$lambda.min.ratio)) {
    args$lambda.min.ratio <- 1e-3
  }
  do.call(
    huge::huge,
    c(args, list(x = data, method = method, verbose = verbose, cov.output = cov.output))
  )
}

check_pulsar_params_local <- function(fun, args = list()) {
  if (!inherits(args, "list") || (length(args) > 0 && is.null(names(args))) || any("" %in% names(args))) {
    stop("pulsar.params must be a named list")
  }
  if (length(args) == 0) {
    return(TRUE)
  }
  fun <- match.fun(fun)
  forms <- formals(fun)
  nargs <- c("data", "fun", "fargs", "criterion")
  extrargs <- intersect(names(args), nargs)
  if (length(extrargs) > 0) {
    stop(sprintf("disallowed arguments to 'pulsar.params': %s", paste(extrargs, collapse = ", ")))
  }
  allforms <- setdiff(names(forms), nargs)
  extrargs <- setdiff(names(args), allforms)
  if (length(extrargs) > 0) {
    stop(sprintf("unrecognized arguments to 'pulsar.params': %s", paste(extrargs, collapse = ", ")))
  }
  TRUE
}

get_opt_index_local <- function(est) {
  est$select$stars$opt.index
}

get_refit_local <- function(est) {
  Matrix::drop0(est$refit$stars)
}

get_opt_cov_local <- function(est) {
  idx <- get_opt_index_local(est)
  Matrix::drop0(est$est$cov[[idx]])
}

get_opt_icov_local <- function(est) {
  idx <- get_opt_index_local(est)
  Matrix::drop0(est$est$icov[[idx]])
}

get_opt_beta_local <- function(est) {
  idx <- get_opt_index_local(est)
  Matrix::drop0(est$est$beta[[idx]])
}

symBeta_local <- function(beta, mode = "maxabs") {
  t_beta <- Matrix::t
  if (nrow(beta) != ncol(beta)) {
    stop("expecting a square matrix")
  }
  if (mode == "ave") {
    symbeta <- (beta + t_beta(beta)) / 2
  } else if (mode == "maxabs") {
    upt <- Matrix::triu(beta)
    lot <- t_beta(Matrix::tril(beta))
    suppressMessages(maxt <- pmax(abs(upt), abs(lot)))
    uptind <- Matrix::which(maxt == abs(upt))
    lotind <- Matrix::which(maxt == abs(lot))
    if (length(uptind) != 0) {
      maxt[uptind] <- maxt[uptind] * sign(upt[uptind])
    }
    if (length(lotind) != 0) {
      maxt[lotind] <- maxt[lotind] * sign(lot[lotind])
    }
    symbeta <- maxt + t_beta(maxt)
  } else {
    stop("mode not recognized")
  }
  as(symbeta, "symmetricMatrix")
}

precision_to_partial_corr <- function(theta) {
  theta <- as.matrix(theta)
  d <- sqrt(diag(theta))
  pcor <- -theta / outer(d, d)
  diag(pcor) <- 1
  storage.mode(pcor) <- "double"
  pcor
}


#' SPIEC-EASI fit (Rcpp-accelerated CLR)
#'
#' Same as \code{spieceasi_fit} but uses fast C++ CLR transformation.
#' @keywords internal
spieceasi_fit_rcpp <- function(
  data,
  method = "glasso",
  sel.criterion = "stars",
  verbose = TRUE,
  pulsar.select = TRUE,
  pulsar.params = list(),
  lambda.log = TRUE,
  ...
) {
  args <- list(...)
  if (!is.matrix(data)) {
    data <- as.matrix(data)
  }
  if (!is.numeric(data)) {
    stop("data must be a numeric matrix")
  }

  if (method %in% c("glasso", "mb")) {
    X <- spieceasi_norm_cpp(data)
    rownames(X) <- colnames(data)
    colnames(X) <- rownames(data)
    estFun <- "sparseiCov_local"
    args$method <- method
    if (is.null(args[["lambda.max"]])) {
      args$lambda.max <- pulsar::getMaxCov(cor(X))
    }
  } else {
    stop("standalone script currently supports only method = 'glasso' or 'mb'")
  }

  if (is.null(args[["lambda"]])) {
    if (is.null(args[["lambda.min.ratio"]])) {
      args$lambda.min.ratio <- 1e-3
    }
    if (is.null(args[["nlambda"]])) {
      args$nlambda <- 20
    }
    args$lambda <- pulsar::getLamPath(
      args$lambda.max,
      args$lambda.max * args$lambda.min.ratio,
      args$nlambda,
      log = lambda.log
    )
    args$lambda.min.ratio <- NULL
    args$nlambda <- NULL
    args$lambda.max <- NULL
  }

  if (!is.null(pulsar.params[["data"]])) {
    stop("supply data directly to spieceasi_fit_rcpp, not pulsar.params")
  }
  if (!is.null(pulsar.params[["criterion"]])) {
    stop("supply sel.criterion directly to spieceasi_fit_rcpp, not pulsar.params")
  }

  if (isTRUE(pulsar.select)) {
    check_pulsar_params_local(pulsar::pulsar, pulsar.params)
    pulsar.params$criterion <- switch(
      sel.criterion,
      stars = "stars",
      bstars = "stars",
      stop("unknown selection criterion")
    )
    if (sel.criterion == "bstars") {
      pulsar.params$lb.stars <- TRUE
      pulsar.params$ub.stars <- TRUE
    }
    if (is.null(pulsar.params[["thresh"]])) {
      pulsar.params$thresh <- 0.05
    }

    if (verbose) {
      message("Applying data transformations (Rcpp) and selecting model with pulsar...")
    }
    est <- do.call(
      pulsar::pulsar,
      c(list(data = X, fun = match.fun(estFun), fargs = args), pulsar.params)
    )
    fit <- suppressWarnings(pulsar::refit(est))
    fit$select <- est
  } else {
    est <- do.call(match.fun(estFun), c(list(data = X), args))
    fit <- list(
      est = est,
      refit = list(stars = est$path[[length(est$path)]]),
      lambda = args$lambda
    )
    class(fit) <- "standalone_spieceasi_fit"
  }

  fit$lambda <- args$lambda
  fit
}


#' SPIEC-EASI matrix (Rcpp implementation)
#'
#' Fast C++-accelerated SPIEC-EASI. Uses Rcpp for CLR transformation;
#' \code{huge} and \code{pulsar} remain unchanged. Input: samples x taxa matrix.
#'
#' @param data Numeric matrix. Rows = samples, columns = taxa (ASVs/OTUs).
#' @param method Character. "glasso" or "mb".
#' @param output Character. One of "partial_correlation", "adjacency", "covariance",
#'   "correlation", "precision", "beta", "stability".
#' @param ... Passed to \code{spieceasi_fit_rcpp} (e.g. \code{pulsar.select},
#'   \code{pulsar.params}, \code{verbose}).
#'
#' @return Matrix as requested by \code{output}.
#' @export
#'
#' @examples
#' \dontrun{
#' pcor_mat <- spieceasi_matrix_rcpp(asv_mat, method = "glasso", output = "partial_correlation")
#' adj_mat  <- spieceasi_matrix_rcpp(asv_mat, method = "glasso", output = "adjacency")
#' }
spieceasi_matrix_rcpp <- function(
  data,
  method = "glasso",
  output = c("partial_correlation", "adjacency", "covariance", "correlation", "precision", "beta", "stability"),
  ...
) {
  output <- match.arg(output)
  fit <- spieceasi_fit_rcpp(data = data, method = method, ...)

  if (output == "adjacency") {
    out <- as.matrix(get_refit_local(fit)) * 1
    dimnames(out) <- list(colnames(data), colnames(data))
    return(out)
  }
  if (output == "stability") {
    if (is.null(fit$select)) {
      stop("stability matrix requires pulsar.select = TRUE")
    }
    idx <- get_opt_index_local(fit)
    out <- as.matrix(Matrix::drop0(fit$select$stars$merge[[idx]]))
    dimnames(out) <- list(colnames(data), colnames(data))
    return(out)
  }
  if (method == "glasso") {
    if (output == "covariance") {
      out <- as.matrix(get_opt_cov_local(fit))
      dimnames(out) <- list(colnames(data), colnames(data))
      return(out)
    }
    if (output == "correlation") {
      out <- cov2cor(as.matrix(get_opt_cov_local(fit)))
      dimnames(out) <- list(colnames(data), colnames(data))
      return(out)
    }
    if (output == "precision") {
      out <- as.matrix(get_opt_icov_local(fit))
      dimnames(out) <- list(colnames(data), colnames(data))
      return(out)
    }
    if (output == "partial_correlation") {
      out <- precision_to_partial_corr(get_opt_icov_local(fit))
      dimnames(out) <- list(colnames(data), colnames(data))
      return(out)
    }
  }
  if (method == "mb" && output == "beta") {
    out <- as.matrix(symBeta_local(get_opt_beta_local(fit), mode = "maxabs"))
    dimnames(out) <- list(colnames(data), colnames(data))
    return(out)
  }
  stop("requested output is not supported for this method")
}
