#' Extract regression results as a table row
#'
#' Builds a one-row data frame containing coefficient estimates and summary
#' statistics from a fitted model.  Clustered or bootstrap standard errors can
#' be used if requested.
#'
#' @param my_lm A fitted model object.
#' @param clust_id Optional name of the clustering variable for clustered
#'   standard errors.
#' @param iv_round Number of digits to round estimates and statistics.
#' @param NAME Column name to use for the resulting row.
#' @param iv Logical; set to `TRUE` for instrumental variable models to include
#'   diagnostic statistics.
#' @param inParens Either "tstat" (default) or "se" indicating whether t-statistics
#'   or standard errors should be placed in parentheses.
#' @param seType Type of standard error.  Either "analytical" or "boot" for
#'   bootstrap.
#' @param bootDataLocation Folder containing bootstrap replications.
#' @param bootDataNameTag File prefix for bootstrap data.
#' @param bootFactorVars Character vector of factor variables to treat as factors
#'   when bootstrapping.
#' @param bootExcludeCovars Variables to exclude when bootstrapping.
#' @param superunit_covariateName Name of the grouping variable used to count
#'   higher level units.
#' @param superunit_label Label to display for the super-unit count.
#' @return A one-row data frame with formatted coefficients and metadata.
#' @export

GetTableEntry <- function(my_lm,
                          clust_id,
                          iv_round = 2,
                          NAME = "",
                          iv = F,
                          inParens = "tstat",
                          seType = "analytical",
                          bootDataLocation = "./",
                          bootDataNameTag = "Data",
                          bootFactorVars = NULL,
                          bootExcludeCovars = NULL,
                          superunit_covariateName = "country",
                          superunit_label = "Countries"
                          ){
  library( sandwich );library( lmtest ); ivDiagnostics <- NULL
  if(seType != "boot"){
    if(is.null(clust_id)){ 
      if(length(coef(my_lm)) > 1){ my_summary <- my_summary_orig <- coeftest(my_lm, vcov. = (VCOV <- vcovHC(my_lm, type = "HC1")) )[-1,] }
      if(length(coef(my_lm)) == 1){ my_summary <- my_summary_orig <- coeftest(my_lm, vcov. = (VCOV <- vcovHC(my_lm, type = "HC1")) ) }
    }
    if(!is.null(clust_id)){ 
      if(length(coef(my_lm)) > 1){ my_summary <- my_summary_orig <- coeftest(my_lm, vcov. = (VCOV <- vcovCluster(my_lm, clust_id)))[-1,] }
      if(length(coef(my_lm)) == 1){ my_summary <- my_summary_orig <- coeftest(my_lm, vcov. = (VCOV <- vcovCluster(my_lm, clust_id))) }
    }
    if("numeric" %in% class(my_summary)){my_summary<-t(my_summary);my_summary_orig<-t(my_summary_orig)}
    if(is.null(row.names(my_summary))){row.names(my_summary) <- row.names(coef(summary(my_lm)))[2]}
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
    boot_pool <- list.files(  bootDataLocation  )
    boot_pool <- boot_pool[grepl(boot_pool, pattern = bootDataNameTag)]
    real_dat <- boot_pool[boot_pool==sprintf("%s_0.csv", bootDataNameTag)]
    DatFinal_boot <- processDataFxn ( DatFinal_boot_init <- read.csv(sprintf("%s/%s",bootDataLocation,real_dat))[,-1] )

    lmCall <- as.character( my_lm$call ); formulaComp <- lmCall[2]
    #if(any(grepl(lmCall,pattern="DatFinal_ethnicityGender"))){
      #DatFinal_boot <- DatFinal_boot_init[DatFinal_boot_init$groupType %in% "ethnicityGender",]
      #processingInstructions <- gsub(lmCall[3], pattern="DatFinal_ethnicityGender", replace="DatFinal_boot") }
    #if(all(!grepl(lmCall,pattern="DatFinal_ethnicityGender"))){processingInstructions <- gsub(lmCall[3], pattern="DatFinal", replace="DatFinal_boot") }
    trimCols <- function(xz){xz}
    covariate_names <- gsub(strsplit(formulaComp,split="~")[[1]][2],pattern="f2n",replace="")
    covariate_names <- gsub(covariate_names,pattern="log\\(",replace="")
    covariate_names <- gsub(covariate_names,pattern="as\\.factor\\(",replace="")
    covariate_names <- gsub(covariate_names,pattern="\\(",replace="")
    covariate_names <- gsub(covariate_names,pattern="\\)",replace="")
    covariate_names <- gsub(covariate_names,pattern=" ",replace="")
    covariate_names <- unlist(strsplit(covariate_names,split="\\+"))

    covariate_names <- covariate_names[!grepl(covariate_names,pattern="\\*")] # drop interactions (must include main terms)
    covariate_names <- covariate_names[ !covariate_names %in% bootExcludeCovars ]
    covariate_names <- covariate_names[covariate_names!='0']

    bootProcessText <- "{
    DatFinal_boot <- eval(parse(text = processingInstructions))
    if(!is.null(bootFactorVars)){ for(factor_ in bootFactorVars){ DatFinal_boot[,factor_] <- as.factor(DatFinal_boot[,factor_] ) } }
    DatFinal_boot_imp <-  try(DatFinal_boot[,covariate_names ],T)
    if(all(class(DatFinal_boot_imp)=='try-error')){print('boot imp error 1');browser()}
    if(all(class(DatFinal_boot_imp)%in%c('numeric','character','factor','integer'))){DatFinal_boot_imp<-as.matrix(DatFinal_boot_imp)}
    if(ncol(DatFinal_boot_imp)>1){
      DatFinal_boot_imp <- try(missForest::missForest(DatFinal_boot[,covariate_names])$ximp,T)
    }
    if(all(class(DatFinal_boot_imp) == 'try-error')){browser()}
    if(length(c(bootExcludeCovars,covariate_names)) != (length(bootExcludeCovars)+ncol(DatFinal_boot_imp))){print('boot error 3');browser()}
    DatFinal_boot[,c(bootExcludeCovars,covariate_names)] <- cbind(DatFinal_boot[,bootExcludeCovars],DatFinal_boot_imp)
    }"
    eval(parse(text = bootProcessText ))
    my_lm <- try(lm(formulaComp,DatFinal_boot),T)
    if("try-error" %in% class(my_lm)){
      # deals with factors that don't show up in bootstrapped data
      formula_names <- formula_names_orig <- strsplit(formulaComp,split="\\~")
      formula_names <- formula_names[[1]][2]
      formula_names <- strsplit(formula_names, split = "\\+")[[1]]
      formula_names <- gsub(formula_names,pattern=" ",replace = "")
      formula_names <- gsub(formula_names,pattern="log\\(",replace = "")
      formula_names <- gsub(formula_names,pattern="as.factor\\(",replace = "")
      formula_names <- gsub(formula_names,pattern="f2n\\(",replace = "")
      formula_names <- gsub(formula_names,pattern="\\)",replace = "")
      formula_length_unique <- apply(DatFinal_boot[,formula_names],2,function(er){length(unique(er))})

      formula_reconstruct <- list(list(),list())
      formula_reconstruct[[1]] <- formula_names_orig[[1]][1]
      formula_reconstruct[[2]] <- strsplit(formula_names_orig[[1]][2],split="\\+")[[1]][formula_length_unique!=1]
      formula_reconstruct[[2]] <- paste(formula_reconstruct[[2]] ,collapse= "+")
      formula_reconstruct <- paste(formula_reconstruct[[1]],formula_reconstruct[[2]],sep="~")
      my_lm <- try(lm(formula_reconstruct,DatFinal_boot),T)
      if('try-error' %in% class(my_lm)){browser()}
    }
    if(ncol(my_lm$model) > 2){ my_summary <- my_summary_orig <- coeftest(my_lm, vcov. = vcovCluster(my_lm, clust_id))[-1,]}
    if(ncol(my_lm$model) == 2){ my_summary <- my_summary_orig <- coeftest(my_lm, vcov. = vcovCluster(my_lm, clust_id))}
    if("numeric" %in% class(my_summary)){my_summary<-t(my_summary)}
    if("numeric" %in% class(my_summary_orig)){my_summary_orig<-t(my_summary_orig)}
    if(is.null(row.names(my_summary))){row.names(my_summary) <- row.names(coef(summary(my_lm)))[2]}

    boot_pool <- boot_pool[boot_pool!="DatFinal_0.csv"]
    coef_mat  <- matrix(NA,ncol=nrow(my_summary),nrow=length(boot_pool))
    colnames(coef_mat) <- row.names( my_summary )
    counter___ <- 0; for(boot__ in boot_pool){
      counter___ <- counter___ + 1
      DatFinal_boot <- processDataFxn ( DatFinal_boot_init <- read.csv(sprintf("%s/%s",bootDataLocation,boot__))[,-1] )

      # deal with special case
      if(any(grepl(lmCall,pattern="DatFinal_ethnicityGender"))){
        DatFinal_boot <- DatFinal_boot_init[DatFinal_boot_init$groupType %in% "ethnicityGender",]
      }

      #new_call <- paste(as.character(my_lm$call$formula)[-1],collapse="~")
      #boot_lm <- eval(parse(text = sprintf("lm(%s,data = DatFinal_boot)",formulaComp)))
      eval(parse(text = bootProcessText ))
      boot_lm <- try(lm(formulaComp, DatFinal_boot),T)
      if("try-error" %in% class(boot_lm)){
        # deals with factors that don't show up in bootstrapped data
        formula_names <- formula_names_orig <- strsplit(formulaComp,split="\\~")
        formula_names <- formula_names[[1]][2]
        formula_names <- strsplit(formula_names, split = "\\+")[[1]]
        formula_names <- gsub(formula_names,pattern=" ",replace = "")
        formula_names <- gsub(formula_names,pattern="log\\(",replace = "")
        formula_names <- gsub(formula_names,pattern="as.factor\\(",replace = "")
        formula_names <- gsub(formula_names,pattern="f2n\\(",replace = "")
        formula_names <- gsub(formula_names,pattern="\\)",replace = "")
        formula_length_unique <- apply(DatFinal_boot[,formula_names],2,function(er){length(unique(er))})

        formula_reconstruct <- list(list(),list())
        formula_reconstruct[[1]] <- formula_names_orig[[1]][1]
        formula_reconstruct[[2]] <- strsplit(formula_names_orig[[1]][2],split="\\+")[[1]][formula_length_unique!=1]
        formula_reconstruct[[2]] <- paste(formula_reconstruct[[2]] ,collapse= "+")
        formula_reconstruct <- paste(formula_reconstruct[[1]],formula_reconstruct[[2]],sep="~")
        boot_lm <- try(lm(formula_reconstruct,DatFinal_boot),T)
        if('try-error' %in% class(boot_lm)){browser()}
      }
      if(ncol(my_lm$model) > 2){ boot_coefs <- coef(boot_lm)[-1] }
      if(ncol(my_lm$model) == 2){ boot_coefs <- coef(boot_lm) }
      boot_coefs <- boot_coefs[names(boot_coefs) %in% row.names(my_summary)]
      coef_mat[counter___,names(boot_coefs)] <- boot_coefs
    }
    my_summary[,2] <- apply(coef_mat,2,function(ze){sd(ze,na.rm=T)})
    my_summary[,3] <- my_summary[,1]/(my_summary[,2])
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

  isGLM <- "glm" %in% class(my_lm)
  evalTex_nSuperunits <- as.character(my_lm$call)[3+isGLM]
  nSuperunits <- try(length(unique(eval(parse(text = evalTex_nSuperunits))[row.names(my_lm$model),superunit_covariateName])),T)
    
  if(isGLM){
    FitLabel <- "AIC"
    FitMeasure <- summary(my_lm)$aic
  }
  if(!isGLM){
    FitLabel <- "Adjusted R-squared"
    FitMeasure <- summary(my_lm)$adj.r.squared

    if(length(coef(my_lm))==1){
      # assumes outcome is in first position of my_lm$model
      FitMeasure <- 1-sum(my_lm$residuals^2) / sum((my_lm$model[,1] - mean(my_lm$model[,1]))^2 )
    }
  }
  
  meta_data <- cbind(c(FitLabel,"Observations",superunit_label),
                     c(fixZeroEndings(round(FitMeasure,iv_round)), nrow( my_lm$model), nSuperunits ))
  meta_data <-rbind(meta_data,ivDiagnostics)
  final_ <- rbind(content_,meta_data)
  colnames(final_) <- c("mergeVar",NAME)
  final_ <- t(final_); colnames(final_) <-final_[1,]
  final_ <- as.data.frame(t(final_[-1,]))
  return( final_ )
}
