#' Generate publication-ready regression tables
#'
#' Collates a list of regression model objects, extracts their coefficient
#' tables via [GetTableEntry()] and writes LaTeX files using
#' [stargazer::stargazer()].  Optionally, a full table of all covariates is
#' produced in addition to a condensed version.
#'
#' @param reg_list A list of fitted model objects or character strings giving
#'   objects to evaluate.
#' @param clust_id Optional name of the clustering variable.
#' @param seType Type of standard errors: either "analytical" or "boot".
#' @param checkmark_list Optional list of binary indicators to add check-marks.
#' @param addrow_list Optional named list of additional rows to append.
#' @param saveFolder Folder in which to write the LaTeX files.
#' @param nameTag Base name for generated files.
#' @param saveFull Logical; if `TRUE` also produce the full table.
#' @param tabCaption Caption to use for the condensed table.
#' @param model.names Optional vector of model column headings.
#' @param NameConversionMat Optional two-column matrix used to rename row labels.
#' @param DoFullTableKey Logical; if `TRUE` mention the full table in the caption.
#' @param superunit_covariateName Variable used to count higher level units.
#' @param superunit_label Label for that count in the table footnotes.
#' @param font.size, font.size.full Font sizes passed to stargazer for the short
#'   and full tables respectively.
#'
#' @return Invisibly returns `NULL`.  LaTeX files are written to `saveFolder`.
#' @export

Tables2Tex <- function(reg_list, clust_id, seType = "analytical",
                       checkmark_list = NULL, addrow_list = NULL,
                       saveFolder = "./", nameTag = "Table", saveFull = T, tabCaption = "",
                       model.names = NULL, NameConversionMat = NULL, DoFullTableKey = T,
                       superunit_covariateName = "country", 
                       superunit_label = "Countries", 
                       font.size = "footnotesize", inParens = "tstat",
                       font.size.full = "footnotesize"){
  print2("Processing R tables...")
  for(i in 1:length(reg_list)){
    if("character" %in% class(reg_list)){
      eval(parse(text = sprintf("t_%s <- GetTableEntry(%s, clust_id = '%s',
                                seType = seType, inParens = inParens, 
                                superunit_covariateName = superunit_covariateName,
                                superunit_label = superunit_label)",
                              i, reg_list[i], clust_id)))
    }
    if(!"character" %in% class(reg_list)){
      eval(parse(text = sprintf("t_%s <- GetTableEntry(reg_list[[i]], clust_id = %s, 
                                seType = seType, inParens = inParens,
                                superunit_covariateName = superunit_covariateName,
                                superunit_label = superunit_label)",
                                i, ifelse(is.null(clust_id), yes = "NULL", no = paste0("'",clust_id,"'"))) ))
    }
  }
  
  t_ <- eval(parse(text = sprintf("t(plyr::rbind.fill(%s))",
                                  paste(paste("t_",1:length(reg_list),sep=""),collapse=",") )  )); t_[is.na(t_)] <- ""
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
  row.names(t_) <- gsub(row.names(t_),pattern="\\.\\.",replace=" ")
  if( !is.null( NameConversionMat ) ){
    # row.names(t_)[!row.names(t_) %in% NameConversionMat[,2]]
    t_ <- t_[na.omit(match(NameConversionMat[,2], row.names(t_))),]
    if(length(reg_list) == 1){ t_ <- as.matrix(t_) }
  }
  row.names(t_) <- gsub(row.names(t_),pattern="\\.\\.",replace=" ")

  print2("Writing model names...")
  if(is.null(model.names)){ model.names <- paste0("Model ", 1:ncol(t_),  "") }
  colnames(t_) <-  model.names

  print2("Setting up full table...")
  fullModelInfo <- ""; TAB_LAB <- sprintf("tab:Reg%s_SE%s",nameTag, seType)
  if(DoFullTableKey){
    fullModelInfo <- sprintf("Full model results are given in Table \\ref{%s}.",
                             TAB_LAB_FULL <- gsub(TAB_LAB, pattern="tab:", replace="tab:FULL_"))
  }

  print2("Building table via stargazer...")
  stargazer_text <- cleanStars( capture.output( stargazer::stargazer(t_,
                                                         label = TAB_LAB,
                                                         font.size = font.size,
                                                         title = sprintf("%s %s", tabCaption, fullModelInfo ))) )
  names(stargazer_text)<-NULL
  write(stargazer_text, file = gsub(sprintf("%s/tab%s_SE%s.tex",
                                      saveFolder, nameTag, seType), pattern="//",replace="/") )

  t_FULL_input <- t_FULL
  if(saveFull == T){
    print2("Saving full table...")
    t_MadeFull <- FullTransformer(t_FULL = t_FULL_input,
                                  COLNAMES_VEC = model.names)
    t_MadeFull <- t_MadeFull[!duplicated(row.names(t_MadeFull)),]
    stargazer_text <- capture.output( stargazer::stargazer(t_MadeFull,
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
                                             saveFolder, nameTag, seType),pattern="//",replace="/") )
  }
  # Note: Function has no R output (all output is read to disk)
  
  print2("Done with call to Tables2Tex()!")
}
