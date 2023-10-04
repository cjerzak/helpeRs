#' Implements...
#'
#' @usage
#'
#' Tables2Tex(...)
#'
#' @param x Description
#'
#' @return `z` Description
#' @export
#'
#' @details `Tables2Tex` implements...
#'
#' @examples
#'
#' # Perform analysis
#' Tables2Tex()
#'
#' print( TableEntries2Tex )
#'
#' @export
#'
#' @md

Tables2Tex <- function(reg_list, clust_id, seType = "analytical",
                       checkmark_list = NULL,
                       saveFolder = "./", nameTag = "Table", saveFull = T, tabCaption = "",
                       model.names = NULL, NameConversionMat = NULL, DoFullTableKey = T,
                       font.size = "footnotesize",
                       font.size.full = "footnotesize"){
  ########################
  # Process R tables
  for(i in 1:length(reg_list)){
    if("character" %in% class(reg_list)){
      eval(parse(text = sprintf("t_%s <- GetTableEntry(%s, clust_id = '%s', seType = seType)",
                              i, reg_list[i], clust_id)))
    }
    if(!"character" %in% class(reg_list)){
      eval(parse(text = sprintf("t_%s <- GetTableEntry(reg_list[[i]], clust_id = '%s', seType = seType)",
                                i, clust_id)))
    }
  }
  t_ <- eval(parse(text = sprintf("t(rbind.fill(%s))",
                                  paste(paste("t_",1:length(reg_list),sep=""),collapse=",") )  )); t_[is.na(t_)] <- ""
  t_FULL <- t_

  ########################
  # Perform name re-writes
  if(!is.null(NameConversionMat)){
  row.names(t_) <- sapply(row.names(t_),function(zer){
    match_ <- sapply(NameConversionMat[,1],function(zerr){ grepl(zer,pattern=zerr) })
    new_name <- NameConversionMat[which(match_),2]
    if(length(new_name) == 0){new_name <- "DROP"}; return(new_name) } )
  }
  t_ <- t_[row.names(t_) != "DROP",]

  if(!is.null(checkmark_list)){
    t_ <- rbind(t_,
                t(data.frame("Factor..covariates" = rep("",times=length(checkmark_list[[1]])) )))
    for(checkm_i in 1:length(checkmark_list)){
      checkm_bind <- eval(parse(text = sprintf('data.frame("%s" = ifelse( checkmark_list[[checkm_i]] == 1, yes = "checkmark", no = ""))',
                                           names(checkmark_list)[checkm_i])))
      t_ <- rbind(t_, t(checkm_bind) )
    }}

  t_ <- rbind(t_,t(data.frame("Other..statistics" = rep("",times=length(reg_list)) )))
  t_ <- rbind(t_,t(data.frame("space1" = rep("",times=length(reg_list)) )))
  t_ <- rbind(t_,t(data.frame("space2" = rep("",times=length(reg_list))  )))
  row.names(t_) <- gsub(row.names(t_),pattern="\\.\\.",replace=" ")
  if(!is.null( NameConversionMat )){
    row.names(t_)[!row.names(t_) %in% NameConversionMat[,2]]
    t_ <- t_[na.omit(match(NameConversionMat[,2],  row.names(t_))),]
  }

  #t_ <- rbind(t_[1:(grep(row.names(t_),pattern="Executive baseline")-1),],
              #t(data.frame("xxx" = rep("",times=4))),
              #t(data.frame("Body..indicators" = rep("",times=4))),
              #t_[grep(row.names(t_),pattern="Executive baseline"):nrow(t_),])
  #row.names(t_)[row.names(t_) == "Party leader"] <- paste(row.names(t_)[row.names(t_) == "Party leader"]," (", round(tapply(DatFinal$body_n,DatFinal$bodyType, mean)["partyLeader"]), ")",sep = "")
  row.names(t_) <- gsub(row.names(t_),pattern="\\.\\.",replace=" ")

  ####################
  # Write model names
  if(is.null(model.names)){
    model.names <- paste("Model (", 1:ncol(t_),  ")",sep = "")
  }
  colnames(t_) <-  model.names

  ####################
  # Setup full table
  fullModelInfo <- ""
  TAB_LAB <- sprintf("tab:Reg%s_SE%s",nameTag, seType)
  if(DoFullTableKey){
    fullModelInfo <- sprintf("Full model results are given in Table \\ref{%s}.",
                             TAB_LAB_FULL <- gsub(TAB_LAB, pattern="tab:", replace="tab:FULL_"))
  }

  ##############################
  # Build table via stargazer
  stargazer_text <- capture.output( stargazer::stargazer(t_,
                                                         label = TAB_LAB,
                                                         font.size = font.size,
                                                         title = sprintf("%s %s", tabCaption, fullModelInfo )))
  stargazer_text <- cleanStars(stargazer_text)
  names(stargazer_text)<-NULL;stargazer_text
  write(stargazer_text,file = gsub(sprintf("%s/tab%s_SE%s.tex",
                                      saveFolder, nameTag, seType), pattern="//",replace="/") )

  ##############################
  # Save full table if desired
  t_FULL_input <- t_FULL
  if(saveFull == T){
    t_MadeFull <- FullTransformer(t_FULL = t_FULL_input,
                                  COLNAMES_VEC = model.names)
    t_MadeFull <- t_MadeFull[!duplicated(row.names(t_MadeFull)),]
    stargazer_text <- capture.output( stargazer::stargazer(t_MadeFull,
                                                           label = TAB_LAB_FULL,
                                                           font.size = font.size.full,
                                                           title = sprintf("%s", tabCaption ))  )
    stargazer_text <- Stargazer2FullTable( stargazer_text,
                                           fontsize = ifelse(ncol(t_MadeFull) < 5,
                                                             yes = "footnotesize",
                                                             no = "tiny"))
    if(any(grepl(stargazer_text, pattern = "\\.\\."))){
      print("Debug checkpoint"); browser()
    }
    write(stargazer_text,file = gsub(sprintf("%s/FULL_tab%s_SE%s.tex",
                                             saveFolder, nameTag, seType),pattern="//",replace="/") )
  }

  ##############################
  # Note: Function has no R output (all output is read to disk)
  ##############################
}
