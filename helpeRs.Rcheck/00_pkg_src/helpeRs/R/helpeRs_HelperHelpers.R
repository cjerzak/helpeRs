# Global variables to avoid R CMD check notes
utils::globalVariables(c("Var1", "Var2", "Freq"))

#' Cluster-robust covariance matrix estimator
#'
#' Computes a clustered sandwich covariance matrix for a fitted model using the
#' method of Arellano (1987). This provides standard errors that are robust to
#' arbitrary within-cluster correlation.
#'
#' The function applies the degrees-of-freedom correction
#' \eqn{(M/(M-1)) \times ((N-1)/(N-K))}{(M/(M-1)) * ((N-1)/(N-K))} where M is
#' the number of clusters, N is the number of observations, and K is the number
#' of parameters.
#'
#' @param fm A fitted model object (typically from \code{lm()} or \code{glm()}).
#'   For \code{polr} models from \pkg{MASS}, predictions are handled specially.
#' @param clvar Character string giving the name of the clustering variable.
#'   This variable must exist in the data used to fit \code{fm}.
#'
#' @return A K x K covariance matrix where K is the number of model coefficients.
#'
#' @references
#' Arellano, M. (1987). Computing Robust Standard Errors for Within-Groups
#' Estimators. \emph{Oxford Bulletin of Economics and Statistics}, 49(4), 431-434.
#'
#' @seealso \code{\link{GetTableEntry}} which uses this function for clustered
#'   standard errors, \code{\link[sandwich]{vcovHC}} for heteroskedasticity-robust
#'   covariance without clustering
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#' fit <- lm(mpg ~ wt + hp, data = mtcars)
#'
#' # Cluster by number of cylinders
#' V_clust <- vcovCluster(fit, "cyl")
#'
#' # Use with coeftest for clustered standard errors
#' library(lmtest)
#' coeftest(fit, vcov. = V_clust)
#' }
#'
#' @export
vcovCluster <- function(fm, clvar){
  # R-codes (www.r-project.org) for computing
  # clustered-standard errors. Mahmood Arai, Jan 26, 2008.
  # The arguments of the function are:
  # fitted model, cluster1 and cluster2
  # You need to install libraries `sandwich' and `lmtest'
  x <- eval(fm$call$data, envir = parent.frame())
  if ("polr" %in% class(fm)) {
    if (!requireNamespace("MASS", quietly = TRUE)) {
      stop("Package 'MASS' is required for polr models")
    }
    cluster <- x[rownames(stats::predict(fm, type = "probs")), clvar]
  } else {
    cluster <- x[names(stats::predict(fm)), clvar]
  }
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- dim(stats::vcov(fm))[1]
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(sandwich::estfun(fm),2, function(x) tapply(x, cluster, sum))
  vcovCL <- dfc*sandwich::sandwich(fm, meat=crossprod(uj)/N)
  return(vcovCL)
}

#' Convert factors to numeric values
#'
#' Converts a vector to numeric by first coercing to character. This is the
#' correct way to extract numeric values from factors, as direct coercion with
#' \code{as.numeric()} returns factor level indices rather than the underlying
#' values.
#'
#' @param x A vector to convert, typically a factor whose levels are numeric
#'   strings (e.g., \code{factor(c("1.5", "2.3", "3.1"))}).
#'
#' @return A numeric vector. Values that cannot be coerced to numeric become
#'   \code{NA} with a warning.
#'
#' @seealso \code{\link{cols2numeric}} for converting multiple columns of a
#'   data frame
#'
#' @examples
#' # Factor with numeric levels
#' x <- factor(c("1.5", "2.3", "3.1"))
#'
#' # Wrong way - returns level indices (1, 2, 3)
#' as.numeric(x)
#'
#' # Correct way - returns actual values
#' f2n(x)  # Returns c(1.5, 2.3, 3.1)
#'
#' @export
f2n <- function(x){as.numeric(as.character(x))}

#' Widen margins in a LaTeX table string
#'
#' Adds horizontal margin adjustments to a LaTeX table by wrapping the table
#' content in an \code{adjustwidth} environment from the \pkg{ragged2e} package.
#' This is useful when tables are too wide to fit within standard margins.
#'
#' The function extends margins by 0.5 inches on each side, effectively allowing
#' the table to be 1 inch wider than the text width.
#'
#' @param x Character vector containing the LaTeX table code, typically the
#'   output from \code{\link[stargazer]{stargazer}} or \code{\link{Tables2Tex}}.
#'
#' @return A character vector with the modified LaTeX code. The \code{adjustwidth}
#'   environment is inserted inside the \code{table} environment.
#'
#' @section LaTeX Requirements:
#' The output requires the \pkg{ragged2e} package in your LaTeX preamble:
#' \preformatted{\\usepackage{ragged2e}}
#'
#' @seealso \code{\link{Tables2Tex}} for generating LaTeX tables
#'
#' @examples
#' \dontrun{
#' # Read an existing table and widen its margins
#' tex_lines <- readLines("my_table.tex")
#' tex_wide <- WidenMargins(tex_lines)
#' writeLines(tex_wide, "my_table_wide.tex")
#' }
#'
#' @export
WidenMargins <- function(x){
  x = gsub(x,  pattern="\\\\begin\\{table\\}\\[htbp\\]",
           replacement="\\\\begin\\{table\\}[htbp]\\\\begin\\{adjustwidth\\}\\{-.5in\\}\\{-.5in\\}")
  x = gsub(x,  pattern="\\\\end\\{table\\}",
           replacement="\\\\end\\{adjustwidth\\}\\\\end\\{table\\}")
  return(x)
}

#' Ensure numbers have a fixed number of decimal places
#'
#' Pads numeric strings with trailing zeros so that all values have exactly
#' \code{roundAt} digits after the decimal point. This ensures consistent
#' formatting in regression tables where column alignment matters.
#'
#' @param zr A character or numeric vector to process. Numeric values are first
#'   converted to character.
#' @param roundAt Integer specifying the desired number of decimal places.
#'   Default is 2.
#'
#' @return A character vector with values padded to have exactly \code{roundAt}
#'   decimal places. Values without a decimal point receive one followed by
#'   the appropriate number of zeros.
#'
#' @seealso \code{\link{GetTableEntry}} which uses this function to format
#'   coefficient estimates
#'
#' @examples
#' # Pad to 2 decimal places (default)
#' fixZeroEndings(c("1.5", "2", "3.14"))
#' # Returns: c("1.50", "2.00", "3.14")
#'
#' # Pad to 3 decimal places
#' fixZeroEndings(c(1.5, 2.33, 3), roundAt = 3)
#' # Returns: c("1.500", "2.330", "3.000")
#'
#' @export
fixZeroEndings <- function(zr,roundAt=2){
  unlist( lapply(strsplit(as.character(zr),split="\\."),function(l_){
    if(length(l_) == 1){ retl <- paste(l_, paste(rep("0",times=roundAt),collapse=""),sep=".") }
    if(length(l_) == 2){
      retl <- paste(l_[1], paste(l_[2], paste(rep("0",times=roundAt-nchar(l_[2])),collapse=""),sep=""),
                    sep = ".") }
    return( retl  )
  }) ) }

##
cleanStars <- function(zer){
  zer = sapply(zer,function(sa)gsub(sa,pattern="\\} c", replacement="\\} l"))
  zer = sapply(zer,function(sa)gsub(sa,pattern="!htbp",replacement="htbp"))
  zer = sapply(zer,function(sa)gsub(sa,pattern="checkmark",replacement="\\\\checkmark"))
  zer = sapply(zer,function(sa)gsub(sa,pattern="\\\\textasteriskcentered",replacement="$^\\*$"))
  zer <- gsub(zer,pattern="Continuous covariates",replacement="\\\\emph{Continuous covariates}")
  zer <- gsub(zer,pattern="Instruments",replacement="\\\\emph{Instruments}")
  
  zer <- gsub(zer,pattern="StartEmph",replacement="\\\\emph{")
  zer <- gsub(zer,pattern="EndEmph",replacement="}")
  
  zer <- gsub(zer,pattern="StartMakeCell",replacement="\\\\makecell{")
  zer <- gsub(zer,pattern="EndMakeCell",replacement="}")
  
  zer <- gsub(zer,pattern="LINEBREAK",replacement=" \\\\\\\\ ")
              
  zer <- gsub(zer,pattern="Factor covariates",replacement="\\\\emph{Factor covariates}")
  zer <- gsub(zer,pattern="Control variables",replacement="\\\\emph{Control variables}")
  zer <- gsub(zer,pattern="Other statistics",replacement="\\\\emph{Other statistics}")
  zer <- gsub(zer,pattern="Body indicators",replacement="\\\\emph{Body indicators}")
  zer <- gsub(zer,pattern="xxx",replacement=" ")
  zer <- gsub(zer,pattern="space0",replacement=" ")
  zer <- gsub(zer,pattern="space1",replacement=" ")
  zer <- gsub(zer,pattern="space2",replacement=" ")
  zer <- gsub(zer,pattern="space3",replacement=" ")
  zer <- gsub(zer,pattern="space4",replacement=" ")
  zer <- gsub(zer,pattern="space5",replacement=" ")
  zer <- gsub(zer,pattern="spaceX",replacement=" ")
  names(zer) <- NULL

  # Find required pattern indices
  captionIdx <- grep(zer, pattern = "caption\\{")
  labelIdx <- grep(zer, pattern = "label\\{")
  insertCaptionAfter <- grep(zer, pattern = "end\\{tabular")


  # Only re-order if all required patterns are found
  if (length(captionIdx) > 0 && length(labelIdx) > 0 && length(insertCaptionAfter) > 0) {
    captionIndices <- captionIdx[1]:labelIdx[1]
    insertCaptionAfter <- insertCaptionAfter[1]

    # re-order so captions are at bottom of table
    zer <- c(zer[1:(min(captionIndices)-1)],
             zer[(max(captionIndices)+1):(insertCaptionAfter)],
             zer[captionIndices],
             zer[(insertCaptionAfter+1):length(zer)])
  }
  return( zer )
}

print2 <- function(text, quiet = F){
  if(!quiet){ print( sprintf("[%s] %s" ,format(Sys.time(), "%Y-%m-%d %H:%M:%S"),text) ) }
}

helpeRsRegexEscape <- function(text){
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", text)
}

helpeRsNormalizeBootstrapArgs <- function(bootstrap_source = "auto",
                                          bootstrap_reps = 999L,
                                          bootstrap_seed = NULL,
                                          bootstrap_dir = NULL,
                                          bootstrap_prefix = NULL,
                                          bootDataLocation = "./",
                                          bootDataNameTag = "Data",
                                          bootFactorVars = NULL,
                                          bootExcludeCovars = NULL){
  bootstrap_source <- match.arg(bootstrap_source, c("auto", "memory", "files"))
  bootstrap_reps <- as.integer(bootstrap_reps)
  if(length(bootstrap_reps) != 1L || is.na(bootstrap_reps) || bootstrap_reps < 1L){
    stop("`bootstrap_reps` must be a positive integer.", call. = FALSE)
  }

  using_legacy_location <- !identical(bootDataLocation, "./")
  using_legacy_prefix <- !identical(bootDataNameTag, "Data")
  using_legacy_processing <- !is.null(bootFactorVars) || !is.null(bootExcludeCovars)

  if(using_legacy_location || using_legacy_prefix){
    warning(
      "`bootDataLocation` and `bootDataNameTag` are deprecated; use `bootstrap_dir` and `bootstrap_prefix` instead.",
      call. = FALSE
    )
  }
  if(using_legacy_processing){
    warning(
      "`bootFactorVars` and `bootExcludeCovars` are deprecated and ignored in the new bootstrap workflow.",
      call. = FALSE
    )
  }

  if(bootstrap_source == "auto"){
    if(!is.null(bootstrap_dir) || !is.null(bootstrap_prefix) || using_legacy_location || using_legacy_prefix){
      bootstrap_source <- "files"
    } else {
      bootstrap_source <- "memory"
    }
  }

  if(bootstrap_source == "files"){
    if(is.null(bootstrap_dir)){ bootstrap_dir <- bootDataLocation }
    if(is.null(bootstrap_prefix)){ bootstrap_prefix <- bootDataNameTag }
    if(length(bootstrap_dir) != 1L || is.na(bootstrap_dir) || !dir.exists(bootstrap_dir)){
      stop("`bootstrap_dir` must point to an existing directory for file-based bootstrap inference.", call. = FALSE)
    }
    if(length(bootstrap_prefix) != 1L || is.na(bootstrap_prefix) || !nzchar(bootstrap_prefix)){
      stop("`bootstrap_prefix` must be a non-empty string for file-based bootstrap inference.", call. = FALSE)
    }
  }

  list(
    source = bootstrap_source,
    reps = bootstrap_reps,
    seed = bootstrap_seed,
    dir = bootstrap_dir,
    prefix = bootstrap_prefix
  )
}

helpeRsValidateBootstrapModel <- function(fm){
  if("glm" %in% class(fm)){ return(invisible(TRUE)) }
  if("lm" %in% class(fm)){ return(invisible(TRUE)) }
  stop("Bootstrap inference currently supports only `lm` and `glm` objects.", call. = FALSE)
}

helpeRsGetOriginalData <- function(fm){
  data_expr <- fm$call$data
  if(is.null(data_expr)){ return(NULL) }

  env_candidates <- list(
    environment(stats::formula(fm)),
    parent.frame(),
    globalenv()
  )
  for(env_ in env_candidates){
    if(is.null(env_)){ next }
    out <- try(eval(data_expr, envir = env_), silent = TRUE)
    if(!inherits(out, "try-error")){ return(out) }
  }
  NULL
}

helpeRsBootstrapTemplateData <- function(fm, clust_id = NULL){
  template_data <- stats::model.frame(fm)
  if(is.null(clust_id) || clust_id %in% names(template_data)){ return(template_data) }

  original_data <- helpeRsGetOriginalData(fm)
  if(is.null(original_data) || !clust_id %in% names(original_data)){
    stop(
      sprintf("Cluster variable `%s` is not available in the fitted model data.", clust_id),
      call. = FALSE
    )
  }

  row_index <- match(row.names(template_data), row.names(original_data))
  if(any(is.na(row_index))){
    stop("Unable to align the fitted model rows with the original data for cluster bootstrap.", call. = FALSE)
  }
  template_data[[clust_id]] <- original_data[row_index, clust_id]
  template_data
}

helpeRsReferenceCoefficients <- function(fm, keepCoef1 = FALSE){
  ref_coefs <- stats::coef(fm)
  if(length(ref_coefs) > 1L && !keepCoef1){
    ref_coefs <- ref_coefs[-1]
  }
  if(is.null(names(ref_coefs))){
    stop("Bootstrap inference requires named coefficients.", call. = FALSE)
  }
  ref_coefs
}

helpeRsBootstrapSample <- function(template_data, clust_id = NULL){
  if(is.null(clust_id)){
    sample_index <- sample.int(nrow(template_data), size = nrow(template_data), replace = TRUE)
    return(template_data[sample_index, , drop = FALSE])
  }

  if(!clust_id %in% names(template_data)){
    stop(sprintf("Cluster variable `%s` is not available for bootstrap resampling.", clust_id), call. = FALSE)
  }

  cluster_factor <- factor(template_data[[clust_id]], exclude = NULL)
  cluster_rows <- split(seq_len(nrow(template_data)), cluster_factor, drop = TRUE)
  sampled_clusters <- sample(seq_along(cluster_rows), size = length(cluster_rows), replace = TRUE)
  sample_index <- unlist(cluster_rows[sampled_clusters], use.names = FALSE)
  template_data[sample_index, , drop = FALSE]
}

helpeRsMaybeStripRowNamesColumn <- function(dat, template_names){
  if(!is.data.frame(dat) || ncol(dat) == 0L){ return(dat) }

  rowname_like <- names(dat)[1] %in% c("X", "...1", "row.names")
  if(rowname_like && all(template_names %in% names(dat)[-1])){
    dat <- dat[-1]
  }
  dat
}

helpeRsAlignBootstrapData <- function(dat, template_data){
  dat <- helpeRsMaybeStripRowNamesColumn(dat, names(template_data))
  missing_cols <- setdiff(names(template_data), names(dat))
  if(length(missing_cols) > 0){
    stop(
      sprintf("Bootstrap data is missing required columns: %s", paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  for(col_ in names(template_data)){
    template_col <- template_data[[col_]]
    if(is.factor(template_col)){
      dat[[col_]] <- factor(dat[[col_]], levels = levels(template_col), ordered = is.ordered(template_col))
      next
    }
    if(is.logical(template_col)){
      dat[[col_]] <- as.logical(dat[[col_]])
      next
    }
    if(is.integer(template_col)){
      dat[[col_]] <- as.integer(dat[[col_]])
      next
    }
    if(is.numeric(template_col)){
      dat[[col_]] <- as.numeric(dat[[col_]])
      next
    }
    if(is.character(template_col)){
      dat[[col_]] <- as.character(dat[[col_]])
    }
  }
  dat
}

helpeRsRefitBootstrapModel <- function(fm, sampled_data, template_data){
  sampled_data <- helpeRsAlignBootstrapData(sampled_data, template_data)
  fit_args <- list(
    formula = stats::formula(fm),
    data = sampled_data
  )

  if("(weights)" %in% names(template_data)){
    fit_args$weights <- sampled_data[["(weights)"]]
  }
  if("(offset)" %in% names(template_data)){
    fit_args$offset <- sampled_data[["(offset)"]]
  }

  if("glm" %in% class(fm)){
    fit_args$family <- fm$family
    if(!is.null(fm$control)){ fit_args$control <- fm$control }
    if(!is.null(fm$method)){ fit_args$method <- fm$method }
    return(do.call(stats::glm, fit_args))
  }

  if(!is.null(fm$method)){ fit_args$method <- fm$method }
  do.call(stats::lm, fit_args)
}

helpeRsAlignCoefficients <- function(fm, coef_names){
  boot_coefs <- stats::coef(fm)
  aligned <- stats::setNames(rep(NA_real_, length(coef_names)), coef_names)
  common_names <- intersect(names(boot_coefs), coef_names)
  aligned[common_names] <- unname(boot_coefs[common_names])
  aligned
}

helpeRsBootstrapFileSet <- function(bootstrap_dir, bootstrap_prefix, bootstrap_reps){
  escaped_prefix <- helpeRsRegexEscape(bootstrap_prefix)
  file_pattern <- paste0("^", escaped_prefix, "_([0-9]+)\\.csv$")
  files <- list.files(bootstrap_dir, pattern = file_pattern, full.names = TRUE)
  if(length(files) == 0L){
    stop(
      sprintf("No bootstrap replicate files matching `%s_*.csv` were found in `%s`.", bootstrap_prefix, bootstrap_dir),
      call. = FALSE
    )
  }

  file_index <- as.integer(sub(file_pattern, "\\1", basename(files)))
  valid_index <- !is.na(file_index)
  files <- files[valid_index]
  file_index <- file_index[valid_index]

  file_order <- order(file_index)
  files <- files[file_order]
  file_index <- file_index[file_order]

  validation_file <- files[file_index == 0L]
  replicate_files <- files[file_index > 0L]
  if(length(replicate_files) == 0L){
    stop(
      sprintf("Bootstrap replicate files `%s_1.csv`, `%s_2.csv`, ... were not found in `%s`.", bootstrap_prefix, bootstrap_prefix, bootstrap_dir),
      call. = FALSE
    )
  }
  if(length(replicate_files) > bootstrap_reps){
    replicate_files <- replicate_files[seq_len(bootstrap_reps)]
  }

  list(
    validation_file = if(length(validation_file) > 0L) validation_file[1] else NULL,
    replicate_files = replicate_files
  )
}

helpeRsBootstrapReplicatesMemory <- function(fm, template_data, clust_id, coef_names, reps, seed = NULL){
  coef_mat <- matrix(NA_real_, nrow = reps, ncol = length(coef_names))
  colnames(coef_mat) <- coef_names

  if(!is.null(seed)){
    old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if(old_seed_exists){
      old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    }
    on.exit({
      if(old_seed_exists){
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)){
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)
    set.seed(seed)
  }

  for(rep_idx in seq_len(reps)){
    sampled_data <- helpeRsBootstrapSample(template_data, clust_id = clust_id)
    refit <- suppressWarnings(try(helpeRsRefitBootstrapModel(fm, sampled_data, template_data), silent = TRUE))
    if(inherits(refit, "try-error")){ next }
    coef_mat[rep_idx, ] <- helpeRsAlignCoefficients(refit, coef_names)
  }
  coef_mat
}

helpeRsBootstrapReplicatesFiles <- function(fm, template_data, coef_names, bootstrap_dir, bootstrap_prefix, bootstrap_reps){
  bootstrap_files <- helpeRsBootstrapFileSet(bootstrap_dir, bootstrap_prefix, bootstrap_reps)
  if(!is.null(bootstrap_files$validation_file)){
    validation_data <- utils::read.csv(bootstrap_files$validation_file, stringsAsFactors = FALSE)
    helpeRsAlignBootstrapData(validation_data, template_data)
  }

  coef_mat <- matrix(NA_real_, nrow = length(bootstrap_files$replicate_files), ncol = length(coef_names))
  colnames(coef_mat) <- coef_names

  for(rep_idx in seq_along(bootstrap_files$replicate_files)){
    bootstrap_data <- utils::read.csv(
      bootstrap_files$replicate_files[rep_idx],
      stringsAsFactors = FALSE
    )
    refit <- suppressWarnings(try(helpeRsRefitBootstrapModel(fm, bootstrap_data, template_data), silent = TRUE))
    if(inherits(refit, "try-error")){ next }
    coef_mat[rep_idx, ] <- helpeRsAlignCoefficients(refit, coef_names)
  }
  coef_mat
}

helpeRsBootstrapSummary <- function(fm,
                                    clust_id = NULL,
                                    keepCoef1 = FALSE,
                                    bootstrap_args){
  helpeRsValidateBootstrapModel(fm)
  template_data <- helpeRsBootstrapTemplateData(fm, clust_id = clust_id)
  ref_coefs <- helpeRsReferenceCoefficients(fm, keepCoef1 = keepCoef1)
  coef_names <- names(ref_coefs)

  coef_mat <- switch(
    bootstrap_args$source,
    memory = helpeRsBootstrapReplicatesMemory(
      fm = fm,
      template_data = template_data,
      clust_id = clust_id,
      coef_names = coef_names,
      reps = bootstrap_args$reps,
      seed = bootstrap_args$seed
    ),
    files = helpeRsBootstrapReplicatesFiles(
      fm = fm,
      template_data = template_data,
      coef_names = coef_names,
      bootstrap_dir = bootstrap_args$dir,
      bootstrap_prefix = bootstrap_args$prefix,
      bootstrap_reps = bootstrap_args$reps
    ),
    stop("Unsupported bootstrap source.", call. = FALSE)
  )

  valid_counts <- colSums(!is.na(coef_mat))
  if(any(valid_counts < 2L)){
    failed_coefs <- names(valid_counts)[valid_counts < 2L]
    stop(
      sprintf("Bootstrap inference requires at least two valid replicate estimates per coefficient. Failed coefficients: %s", paste(failed_coefs, collapse = ", ")),
      call. = FALSE
    )
  }

  boot_se <- apply(coef_mat, 2, stats::sd, na.rm = TRUE)
  boot_tstat <- ref_coefs / boot_se
  summary_mat <- cbind(ref_coefs, boot_se, boot_tstat, rep(NA_real_, length(ref_coefs)))
  row.names(summary_mat) <- coef_names
  colnames(summary_mat) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  summary_mat
}
