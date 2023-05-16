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

Tables2Tex <- function(reg_list, clust_id, seType,
                       saveFolder = "./", nameTag = "Table", saveFull = T, tabCaption = "",
                       model.names = NULL, NameConversionMat = NULL,
                       font.size = "footnote.size",
                       font.size.full = "footnote.size"){
  ########################
  # Process R tables
  for(i in 1:length(reg_list)){
    eval(parse(text = sprintf("t_%s <- GetTableEntry(%s, clust_id = '%s', seType = seType)",
                              i, reg_list[i], clust_id)))
  }
  t_ <- eval(parse(text = sprintf("t(rbind.fill(%s))", paste(paste("t_",1:length(reg_list),sep=""),collapse=",") )  )); t_[is.na(t_)] <- ""
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
  t_ <- rbind(t_,t(data.frame("Executive..baseline" = c("checkmark","","",""))))
  t_ <- rbind(t_,t(data.frame("Factor..covariates" = c("","","",""))))
  #t_ <- rbind(t_,t(data.frame("Continuous..covariates" = c("","","",""))))
  t_ <- rbind(t_,t(data.frame("Identity" = rep("checkmark",times=4))))
  t_ <- rbind(t_,t(data.frame("Body..type" = c("","","checkmark","checkmark"))))
  t_ <- rbind(t_,t(data.frame("Gender..quota..type" = c("checkmark","checkmark","checkmark","checkmark"))))
  t_ <- rbind(t_,t(data.frame("Selection..rule" = c("checkmark","","checkmark","checkmark"))))
  t_ <- rbind(t_,t(data.frame("Round" = c("checkmark","checkmark","checkmark","checkmark"))))
  t_ <- rbind(t_,t(data.frame("Country" = c("checkmark","checkmark","checkmark","checkmark"))))
  t_ <- rbind(t_,t(data.frame("Other..statistics" = c("","","",""))))
  t_ <- rbind(t_,t(data.frame("space1" = c("","","",""))))
  t_ <- rbind(t_,t(data.frame("space2" = c("","","",""))))
  row.names(t_) <- gsub(row.names(t_),pattern="\\.\\.",replace=" ")
  if(!is.null( NameConversionMat )){
    row.names(t_)[!row.names(t_) %in% NameConversionMat[,2]]
    t_ <- t_[na.omit(match(NameConversionMat[,2],  row.names(t_))),]
  }

  t_ <- rbind(t_[1:(grep(row.names(t_),pattern="Executive baseline")-1),],
              t(data.frame("xxx" = rep("",times=4))),
              t(data.frame("Body..indicators" = rep("",times=4))),
              t_[grep(row.names(t_),pattern="Executive baseline"):nrow(t_),])
  row.names(t_)[row.names(t_) == "Executive baseline"] <- "Executive baseline (1-2)"#paste(row.names(t_)[row.names(t_) == "Executive baseline"]," (", round((tapplyRes <- tapply(DatFinal$body_n,DatFinal$bodyType, function(zer){mean(zer,na.rm=T)}))["executive"]), ")",sep = "")
  row.names(t_)[row.names(t_) == "Party leader"] <- paste(row.names(t_)[row.names(t_) == "Party leader"]," (", round((tapplyRes <- tapply(DatFinal$body_n,DatFinal$bodyType, mean))["partyLeader"]), ")",sep = "")
  row.names(t_)[row.names(t_) == "Supreme court"] <- paste(row.names(t_)[row.names(t_) == "Supreme court"]," (", round(tapplyRes["supremeCourt"]), ")",sep = "")
  row.names(t_)[row.names(t_) == "Cabinet"] <- paste(row.names(t_)[row.names(t_) == "Cabinet"]," (", round(tapplyRes["cabinet"]), ")",sep = "")
  row.names(t_)[row.names(t_) == "Party"] <- paste(row.names(t_)[row.names(t_) == "Party"]," (", round(tapplyRes["party"]), ")",sep = "")
  row.names(t_)[row.names(t_) == "Upper house"] <- paste(row.names(t_)[row.names(t_) == "Upper house"]," (", round(tapplyRes["upperHouse"]), ")",sep = "")
  row.names(t_)[row.names(t_) == "Lower house"] <- paste(row.names(t_)[row.names(t_) == "Lower house"]," (", round(tapplyRes["lowerHouse"]), ")",sep = "")
  row.names(t_)[row.names(t_) == "Party leader"] <- paste(row.names(t_)[row.names(t_) == "Party leader"]," (", round(tapply(DatFinal$body_n,DatFinal$bodyType, mean)["partyLeader"]), ")",sep = "")
  row.names(t_) <- gsub(row.names(t_),pattern="\\.\\.",replace=" ")

  ####################
  # Write model names
  if(is.null(model.names)){ model.names <- paste("Model (", 1:ncol(t_),  ")",sep = "") }
  colnames(t_) <-  model.names

  ####################
  # Setup full table
  fullModelInfo <- ""
  TAB_LAB <- sprintf("tab:Reg%s_SE%s",nameTag, seType)
  if(DoFullTableKey){
    fullModelInfo <- sprintf("Full model results are given in Table \\ref{%s}.",
                             TAB_LAB_FULL <- gsub(TAB_LAB,pattern="tab:",replace="tab:FULL_"))
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
                                      saveFolder, nameTag, seType),pattern="//",replace="/") )

  ##############################
  # Save full table if desired
  if(saveFull == T){
    stargazer_text <- capture.output( stargazer::stargazer(TRANSFORM_TABLES_FULL(t_FULL,model.names),
                                                           label = TAB_LAB_FULL,
                                                           font.size = font.size.full,
                                                           title = sprintf("%s", tabCaption )))
    stargazer_text <- Stargazer2FullTable( stargazer_text )
    write(stargazer_text,file = gsub(sprintf("%s/FULL_tab%s_SE%s.tex",
                                             saveFolder, nameTag, seType),pattern="//",replace="/") )
  }

  ##############################
  # Note: Function has no R output (all output is read to disk)
}
