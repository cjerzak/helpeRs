#' Extract regression results as a formatted table row
#'
#' Builds a one-row data frame containing coefficient estimates, standard errors
#' or t-statistics, significance stars, and summary statistics from a fitted model.
#' Supports both analytical standard errors (including clustered and
#' heteroskedasticity-robust) and bootstrap-based inference.
#'
#' The output format is designed for downstream processing by \code{\link{Tables2Tex}},
#' with coefficients formatted as "estimate (stat)*" where stat is either the
#' t-statistic or standard error (controlled by \code{inParens}), and * indicates
#' p < 0.05.
#'
#' @param my_lm A fitted model object (typically from \code{lm()}, \code{glm()},
#'   or \code{ivreg()}). The data used to fit the model must be a data.frame.
#' @param clust_id Character string giving the name of the clustering variable
#'   for clustered standard errors. If \code{NULL}, heteroskedasticity-robust
#'   standard errors (HC1) are used instead.
#' @param iv_round Integer specifying the number of decimal places for rounding
#'   estimates and statistics. Default is 2.
#' @param NAME Character string used as the column name for the resulting row.
#'   Default is an empty string.
#' @param iv Logical; if \code{TRUE}, includes instrumental variable diagnostic
#'   statistics (Weak instruments and Wu-Hausman tests) in the output. Only
#'   applicable for models fitted with \code{ivreg()}. Default is \code{FALSE}.
#' @param inParens Character string specifying what to display in parentheses:
#'   \code{"tstat"} (default) for t-statistics or \code{"se"} for standard errors.
#' @param seType Character string specifying the type of standard errors:
#'   \code{"analytical"} (default) uses sandwich estimators, \code{"boot"} uses
#'   bootstrap standard errors from either in-memory resampling or pre-computed
#'   replication datasets.
#' @param bootstrap_source Character string specifying how bootstrap replications
#'   are sourced: \code{"auto"} (default), \code{"memory"}, or \code{"files"}.
#'   Auto mode uses file-based bootstrap when \code{bootstrap_dir} or
#'   \code{bootstrap_prefix} is supplied, and in-memory resampling otherwise.
#' @param bootstrap_reps Integer giving the number of bootstrap replications to
#'   use. For file-based bootstrap, this is the maximum number of replicate
#'   files to load.
#' @param bootstrap_seed Optional integer seed for reproducible in-memory
#'   bootstrap resampling. Ignored for file-based bootstrap.
#' @param bootstrap_dir Character string giving the folder path containing
#'   bootstrap replication datasets when \code{bootstrap_source = "files"}.
#' @param bootstrap_prefix Character string giving the file name prefix for
#'   bootstrap data files. Files should be named
#'   \code{{bootstrap_prefix}_0.csv} for optional validation data and
#'   \code{{bootstrap_prefix}_1.csv}, etc. for replications.
#' @param bootDataLocation Deprecated compatibility alias for
#'   \code{bootstrap_dir}.
#' @param bootDataNameTag Deprecated compatibility alias for
#'   \code{bootstrap_prefix}.
#' @param bootFactorVars Character vector of variable names to treat as factors
#'   during bootstrap estimation. Deprecated and ignored in the new bootstrap
#'   workflow.
#' @param bootExcludeCovars Character vector of covariate names to exclude from
#'   the imputation step during bootstrap processing. Deprecated and ignored in
#'   the new bootstrap workflow.
#' @param keepCoef1 Logical; if \code{TRUE}, includes the first coefficient
#'   (typically the intercept) in the output. Default is \code{FALSE}.
#' @param superunit_covariateName Character string giving the name of the variable
#'   used to count higher-level units (e.g., countries in panel data). Default
#'   is \code{"country"}.
#' @param superunit_label Character string used as the row label for the
#'   higher-level unit count in the output table. Default is \code{"Countries"}.
#'
#' @return A one-row data frame where each column corresponds to a coefficient
#'   or summary statistic. Columns include:
#'   \itemize{
#'     \item Formatted coefficient estimates with significance stars
#'     \item Fit statistic (Adjusted R-squared for linear models, AIC for GLMs)
#'     \item Number of observations
#'     \item Count of higher-level units (e.g., countries)
#'     \item IV diagnostics (if \code{iv = TRUE})
#'   }
#'
#' @seealso \code{\link{Tables2Tex}} for generating complete LaTeX tables from
#'   multiple models, \code{\link{vcovCluster}} for the clustered standard error
#'   implementation
#'
#' @examples
#' \dontrun{
#' # Fit a simple linear model
#' data(mtcars)
#' fit <- lm(mpg ~ wt + hp, data = mtcars)
#'
#' # Extract with robust standard errors
#' entry <- GetTableEntry(fit, clust_id = NULL, NAME = "Model 1")
#'
#' # Extract with clustered standard errors
#' entry_clust <- GetTableEntry(fit, clust_id = "cyl", NAME = "Model 2")
#' }
#'
#' @export

GetTableEntry <- function(my_lm,
                          clust_id,
                          iv_round = 2,
                          NAME = "",
                          iv = F,
                          inParens = "tstat",
                          seType = "analytical",
                          bootstrap_source = "auto",
                          bootstrap_reps = 999L,
                          bootstrap_seed = NULL,
                          bootstrap_dir = NULL,
                          bootstrap_prefix = NULL,
                          bootDataLocation = "./",
                          bootDataNameTag = "Data",
                          bootFactorVars = NULL,
                          bootExcludeCovars = NULL,
                          keepCoef1 = FALSE, 
                          superunit_covariateName = "country",
                          superunit_label = "Countries"
                          ){
  ivDiagnostics <- NULL
  model_for_meta <- my_lm
  if(seType != "boot"){
    if(is.null(clust_id)){
      if(length(stats::coef(my_lm)) > 1 & !keepCoef1){ my_summary <- my_summary_orig <- lmtest::coeftest(my_lm, vcov. = (VCOV <- sandwich::vcovHC(my_lm, type = "HC1")) )[-1,] }
      if(length(stats::coef(my_lm)) == 1 | keepCoef1){ my_summary <- my_summary_orig <- lmtest::coeftest(my_lm, vcov. = (VCOV <- sandwich::vcovHC(my_lm, type = "HC1")) ) }
    }
    if(!is.null(clust_id)){
      if(length(stats::coef(my_lm)) > 1 & !keepCoef1){ my_summary <- my_summary_orig <- lmtest::coeftest(my_lm, vcov. = (VCOV <- vcovCluster(my_lm, clust_id)))[-1,] }
      if(length(stats::coef(my_lm)) == 1 | keepCoef1){ my_summary <- my_summary_orig <- lmtest::coeftest(my_lm, vcov. = (VCOV <- vcovCluster(my_lm, clust_id))) }
    }
    if("numeric" %in% class(my_summary)){my_summary<-t(my_summary);my_summary_orig<-t(my_summary_orig)}
    if(is.null(row.names(my_summary))){row.names(my_summary) <- row.names(stats::coef(summary(my_lm)))[2]}
    if(iv == T){
      Diagnostics <- summary(my_lm, vcov = VCOV, df = Inf, diagnostics = TRUE)
      ivDiagnostics <- Diagnostics$diagnostics[1:2,3]
      ivDiagnostics_names <-names(ivDiagnostics)
      iv_stars <- c("","")
      iv_stars[Diagnostics$diagnostics[1:2,4]<0.05]<-"*"
      ivDiagnostics <-  round(ivDiagnostics,iv_round)
      ivDiagnostics <- paste(ivDiagnostics,iv_stars,sep="")
      ivDiagnostics <- cbind(ivDiagnostics_names,ivDiagnostics)
    }
  }
  if(seType == "boot"){
    if(isTRUE(iv)){
      stop("Bootstrap inference does not support IV diagnostics in v1.", call. = FALSE)
    }
    bootstrap_args <- helpeRsNormalizeBootstrapArgs(
      bootstrap_source = bootstrap_source,
      bootstrap_reps = bootstrap_reps,
      bootstrap_seed = bootstrap_seed,
      bootstrap_dir = bootstrap_dir,
      bootstrap_prefix = bootstrap_prefix,
      bootDataLocation = bootDataLocation,
      bootDataNameTag = bootDataNameTag,
      bootFactorVars = bootFactorVars,
      bootExcludeCovars = bootExcludeCovars
    )
    my_summary <- my_summary_orig <- helpeRsBootstrapSummary(
      fm = my_lm,
      clust_id = clust_id,
      keepCoef1 = keepCoef1,
      bootstrap_args = bootstrap_args
    )
  }
  my_summary <- round(my_summary,iv_round)
  
  # define what to return 
  if(inParens == "tstat"){  my_tab <- my_summary[,c(1,3)] }
  if(inParens == "se"){  my_tab <- my_summary[,c(1,2)] }
  
  if(all(class(my_tab) == "numeric")){my_tab<-t(my_tab)}
  my_tab[,1] <- fixZeroEndings(my_tab[,1])
  my_tab[,2] <- fixZeroEndings(my_tab[,2]) 
  star_key <- rep("",times = nrow(my_summary))
  if(seType != "boot"){ star_key[my_summary_orig[,4]<0.05] <- "*" }
  if(seType == "boot"){
    #lower_ <- apply(coef_mat,2,function(zer){quantile(zer,prob = 0.025,na.rm=T)});upper_ <- apply(coef_mat,2,function(zer){quantile(zer,prob = 0.975,na.rm=T)})
    #star_key[!(lower_ <= 0 & upper_ >= 0)] <- "*"
    star_key[abs(my_summary[,3]) > 1.96] <- "*"
  }
  content_ <- paste(my_tab[,1], " (",my_tab[,2], ")",sep = "") 
  content_ <- paste(content_, star_key, sep = "")
  content_ <- cbind(row.names(my_summary),content_)

  isGLM <- "glm" %in% class(model_for_meta)
  evalTex_nSuperunits <- as.character(model_for_meta$call)[3+isGLM]
  nSuperunits <- try(length(unique(eval(parse(text = evalTex_nSuperunits))[row.names(model_for_meta$model),superunit_covariateName])),T)
    
  if(isGLM){
    FitLabel <- "AIC"
    FitMeasure <- summary(model_for_meta)$aic
  }
  if(!isGLM){
    FitLabel <- "Adjusted R-squared"
    FitMeasure <- summary(model_for_meta)$adj.r.squared

    if(length(stats::coef(model_for_meta))==1){
      # assumes outcome is in first position of my_lm$model
      FitMeasure <- 1-sum(model_for_meta$residuals^2) / sum((model_for_meta$model[,1] - mean(model_for_meta$model[,1]))^2 )
    }
  }
  
  meta_data <- cbind(c(FitLabel,"Observations",superunit_label),
                     c(fixZeroEndings(round(FitMeasure,iv_round)), nrow( model_for_meta$model), nSuperunits ))
  meta_data <-rbind(meta_data,ivDiagnostics)
  final_ <- rbind(content_,meta_data)
  colnames(final_) <- c("mergeVar",NAME)
  final_ <- t(final_); colnames(final_) <-final_[1,]
  final_ <- as.data.frame(t(final_[-1,]))
  return( final_ )
}
