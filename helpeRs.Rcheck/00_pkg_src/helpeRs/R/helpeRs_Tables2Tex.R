#' Generate publication-ready regression tables in LaTeX
#'
#' Collates a list of regression model objects, extracts their coefficient
#' estimates and statistics via \code{\link{GetTableEntry}}, and writes
#' publication-ready LaTeX table files using \code{\link[stargazer]{stargazer}}.
#'
#' This is the main user-facing function for the regression table workflow.
#' It produces two outputs:
#' \enumerate{
#'   \item A condensed table showing only the covariates specified in
#'     \code{NameConversionMat} (or all if \code{NULL})
#'   \item A full table (if \code{saveFull = TRUE}) containing all coefficients
#'     in longtable format for multi-page output
#' }
#'
#' @param reg_list A list of fitted model objects, or a character vector of
#'   object names to evaluate. Models should be fitted using \code{lm()},
#'   \code{glm()}, or \code{ivreg()}. The data argument to these models
#'   must be a data.frame (not a matrix or tibble).
#' @param clust_id Character string giving the name of the clustering variable
#'   for clustered standard errors, or \code{NULL} for heteroskedasticity-robust
#'   standard errors.
#' @param seType Character string specifying the type of standard errors:
#'   \code{"analytical"} (default) or \code{"boot"} for bootstrap.
#' @param bootstrap_source Character string specifying how bootstrap replications
#'   are sourced when \code{seType = "boot"}: \code{"auto"} (default),
#'   \code{"memory"}, or \code{"files"}.
#' @param bootstrap_reps Integer giving the number of bootstrap replications to
#'   run in memory mode, or the maximum number of replicate files to load in
#'   file mode.
#' @param bootstrap_seed Optional integer seed for reproducible in-memory
#'   bootstrap resampling.
#' @param bootstrap_dir Character string giving the folder path containing
#'   bootstrap replication datasets when \code{bootstrap_source = "files"}.
#' @param bootstrap_prefix Character string giving the file name prefix for
#'   bootstrap data files, e.g. \code{{bootstrap_prefix}_1.csv}.
#' @param checkmark_list Optional named list of binary vectors indicating which
#'   models include certain features. Names become row labels, values of 1
#'   produce checkmarks in the table.
#' @param addrow_list Optional named list of vectors to append as additional
#'   rows to the table. Each list element becomes a row with the element name
#'   as the row label.
#' @param saveFolder Character string giving the folder path where LaTeX files
#'   will be written. Default is the current directory.
#' @param nameTag Character string used as the base name for output files.
#'   Files are named \code{tab{nameTag}_SE{seType}.tex} and
#'   \code{FULL_tab{nameTag}_SE{seType}.tex}.
#' @param saveFull Logical; if \code{TRUE} (default), also produces a full table
#'   containing all coefficients using the longtable environment.
#' @param tabCaption Character string for the table caption.
#' @param model.names Optional character vector of column headings for each model.
#'   If \code{NULL}, defaults to "Model 1", "Model 2", etc.
#' @param NameConversionMat Optional two-column matrix for filtering and renaming
#'   row labels. Column 1 contains regex patterns to match, column 2 contains
#'   replacement names. Rows not matching any pattern are dropped from the
#'   condensed table (but retained in the full table).
#' @param DoFullTableKey Logical; if \code{TRUE} (default), adds a reference to
#'   the full table in the condensed table's caption.
#' @param superunit_covariateName Character string giving the variable name used
#'   to count higher-level units (e.g., \code{"country"} for panel data).
#' @param superunit_label Character string for the row label showing the count
#'   of higher-level units. Default is \code{"Countries"}.
#' @param font.size Character string specifying the LaTeX font size for the
#'   condensed table (e.g., \code{"footnotesize"}, \code{"scriptsize"}).
#' @param inParens Character string specifying what to display in parentheses:
#'   \code{"tstat"} (default) or \code{"se"} for standard errors.
#' @param keepCoef1 Logical; if \code{TRUE}, includes the first coefficient
#'   (typically the intercept) in the output. Default is \code{FALSE}.
#' @param font.size.full Character string specifying the LaTeX font size for
#'   the full table. Default is \code{"footnotesize"}.
#'
#' @return Invisibly returns \code{NULL}. The function is called for its side
#'   effect of writing LaTeX files to \code{saveFolder}.
#'
#' @section LaTeX Requirements:
#' The generated tables require the following LaTeX packages:
#' \itemize{
#'   \item \code{longtable} for full tables
#'   \item \code{amssymb} for checkmark symbols
#' }
#'
#' @seealso \code{\link{GetTableEntry}} for extracting individual model results,
#'   \code{\link{FullTransformer}} for table cleaning,
#'   \code{\link{Stargazer2FullTable}} for longtable conversion
#'
#' @examples
#' \dontrun{
#' # Fit multiple models
#' fit1 <- lm(mpg ~ wt, data = mtcars)
#' fit2 <- lm(mpg ~ wt + hp, data = mtcars)
#' fit3 <- lm(mpg ~ wt + hp + cyl, data = mtcars)
#'
#' # Generate tables
#' Tables2Tex(
#'   reg_list = list(fit1, fit2, fit3),
#'   clust_id = NULL,
#'   saveFolder = "./tables/",
#'   nameTag = "MPG_Models",
#'   tabCaption = "Determinants of Fuel Efficiency",
#'   model.names = c("Base", "Controls", "Full")
#' )
#' }
#'
#' @export

Tables2Tex <- function(reg_list, clust_id, seType = "analytical",
                       bootstrap_source = "auto",
                       bootstrap_reps = 999L,
                       bootstrap_seed = NULL,
                       bootstrap_dir = NULL,
                       bootstrap_prefix = NULL,
                       checkmark_list = NULL, addrow_list = NULL,
                       saveFolder = "./", nameTag = "Table", saveFull = T, tabCaption = "",
                       model.names = NULL, NameConversionMat = NULL, DoFullTableKey = T,
                       superunit_covariateName = "country", 
                       superunit_label = "Countries", 
                       font.size = "footnotesize", inParens = "tstat",
                       keepCoef1 = FALSE, # usually intercept 
                       font.size.full = "footnotesize"){
  print2("Processing R tables... [Please ensure input to lm is a data.frame, not matrix or tible]")
  table_entries <- lapply(seq_along(reg_list), function(i){
    reg_obj <- reg_list[[i]]
    if("character" %in% class(reg_list)){
      reg_obj <- eval(parse(text = reg_list[i]))
    }
    GetTableEntry(
      my_lm = reg_obj,
      clust_id = clust_id,
      seType = seType,
      inParens = inParens,
      bootstrap_source = bootstrap_source,
      bootstrap_reps = bootstrap_reps,
      bootstrap_seed = bootstrap_seed,
      bootstrap_dir = bootstrap_dir,
      bootstrap_prefix = bootstrap_prefix,
      superunit_covariateName = superunit_covariateName,
      keepCoef1 = keepCoef1,
      superunit_label = superunit_label
    )
  })

  t_ <- t(plyr::rbind.fill(table_entries))
  t_[is.na(t_)] <- ""
  t_FULL <- t_

  print2("Performing name re-writes...")
  if(!is.null(NameConversionMat)){
  row.names(t_) <- sapply(row.names(t_),function(zer){
    match_ <- sapply(NameConversionMat[,1],function(zerr){ grepl(zer,pattern=zerr) })
    new_name <- NameConversionMat[which(match_),2]
    if(length(new_name) == 0){new_name <- "DROP"}; return(new_name) } )
  }
  t_ <- t_[row.names(t_) != "DROP",]

  if(!is.null(checkmark_list)){
    t_ <- rbind(t_, t(data.frame("Factor..covariates" = rep("",times=length(checkmark_list[[1]])) )))
    for(checkm_i in 1:length(checkmark_list)){
      checkm_bind <- eval(parse(text = sprintf('data.frame("%s" = ifelse( checkmark_list[[checkm_i]] == 1, yes = "checkmark", no = ""))',
                                           names(checkmark_list)[checkm_i])))
      t_ <- rbind(t_, t(checkm_bind) )
    }}
  
  if(!is.null(addrow_list)){
    for(addrow_i in 1:length(addrow_list)){
      t_ <- rbind(t_,
          eval(parse(text = sprintf("t(data.frame('%s' = addrow_list[[addrow_i]]))",
                                    names(addrow_list)[[addrow_i]]) ) ) )
    }
  }
  
  print2("Incorporating other statistics info...")
  if(length(reg_list) == 1){ t_ <- as.matrix(t_) }
  t_ <- rbind(t_,t(data.frame("Other..statistics" = rep("",times=length(reg_list)) )))
  t_ <- rbind(t_,t(data.frame("Control..variables" = rep("",times=length(reg_list)) )))
  t_ <- rbind(t_,t(data.frame("space0" = rep("",times=length(reg_list)) )))
  t_ <- rbind(t_,t(data.frame("space1" = rep("",times=length(reg_list)) )))
  t_ <- rbind(t_,t(data.frame("space2" = rep("",times=length(reg_list))  )))
  t_ <- rbind(t_,t(data.frame("space3" = rep("",times=length(reg_list))  )))
  t_ <- rbind(t_,t(data.frame("space4" = rep("",times=length(reg_list))  )))
  row.names(t_) <- gsub(row.names(t_),pattern="\\.\\.",replacement=" ")
  if( !is.null( NameConversionMat ) ){
    # row.names(t_)[!row.names(t_) %in% NameConversionMat[,2]]
    t_ <- t_[stats::na.omit(match(NameConversionMat[,2], row.names(t_))),]
    if(length(reg_list) == 1){ t_ <- as.matrix(t_) }
  }
  row.names(t_) <- gsub(row.names(t_),pattern="\\.\\.",replacement=" ")

  print2("Writing model names...")
  if(is.null(model.names)){ model.names <- paste0("Model ", 1:ncol(t_),  "") }
  colnames(t_) <-  model.names

  print2("Setting up full table...")
  fullModelInfo <- ""; TAB_LAB <- sprintf("tab:Reg%s_SE%s",nameTag, seType)
  if(DoFullTableKey){
    fullModelInfo <- sprintf("Full model results are given in Table \\ref{%s}.",
                             TAB_LAB_FULL <- gsub(TAB_LAB, pattern="tab:", replacement="tab:FULL_"))
  }

  print2("Building table via stargazer...")
  stargazer_text <- cleanStars( utils::capture.output( stargazer::stargazer(t_,
                                                         label = TAB_LAB,
                                                         font.size = font.size,
                                                         title = sprintf("%s %s", tabCaption, fullModelInfo ))) )
  names(stargazer_text) <- NULL
  write(stargazer_text, file = gsub(sprintf("%s/tab%s_SE%s.tex",
                                      saveFolder, nameTag, seType), pattern="//",replacement="/") )

  t_FULL_input <- t_FULL
  if(saveFull == T){
    print2("Saving full table...")
    t_MadeFull <- FullTransformer(t_FULL = t_FULL_input,
                                  COLNAMES_VEC = model.names,
                                  name_conversion_matrix = NameConversionMat)
    t_MadeFull <- t_MadeFull[!duplicated(row.names(t_MadeFull)),]
    stargazer_text <- utils::capture.output( stargazer::stargazer(t_MadeFull,
                                                           label = TAB_LAB_FULL,
                                                           font.size = font.size.full,
                                                           title = sprintf("%s", tabCaption ))  )
    stargazer_text <- Stargazer2FullTable( stargazer_text,
                                           fontsize = ifelse(ncol(t_MadeFull) < 5,
                                                             yes = "footnotesize", no = "tiny"))
    if(any(grepl(stargazer_text, pattern = "\\.\\."))){
      print("Debug checkpoint"); browser()
    }
    write(stargazer_text,file = gsub(sprintf("%s/FULL_tab%s_SE%s.tex",
                                             saveFolder, nameTag, seType),pattern="//",replacement="/") )
  }
  # Note: Function has no R output (all output is read to disk)
  
  print2("Done with call to Tables2Tex()!")
}
