#' Implements...
#'
#' @usage
#'
#' image2(...)
#'
#' @param x Description
#'
#' @return `z` Description
#' @export
#'
#' @details `image2` implements...
#'
#' @examples
#'
#' # Perform analysis
#' image2()
#'
#' @export
#'
#' @md

image2 = function(mat,xaxt=NULL,yaxt = NULL,col=NULL,main=NULL,scale_vec=c(1,1.04),cex.axis = 1){
  # pretty plotting of 2D data
  image((t(mat)[,nrow(mat):1]),
        axes = F, main = main,xaxs = "i",cex.main = 2)
  if(!is.null(xaxt)){ axis(1, at = 0:(ncol(mat)-1)/ncol(mat)*scale_vec[1], tick=F,labels = (xaxt),cex.axis = cex.axis,las = 1)  }
  if(!is.null(yaxt)){ axis(2, at = 0:(nrow(mat)-1)/nrow(mat)*scale_vec[2], tick=F,labels = rev(yaxt),cex.axis = cex.axis,las = 2)  }
}

#' Implements...
#'
#' @usage
#'
#' heatMap(...)
#'
#' @param x Description
#'
#' @return `z` Description
#' @export
#'
#' @details `image2` implements...
#'
#' @examples
#'
#' # Perform analysis
#' heatMap()
#'
#' @export
#'
#' @md

heatMap <- function(x, y, z,
                    main = "",
                    N,yaxt = NULL,
                    xlab = "",
                    ylab ="",horizontal = F,
                    useLog = "", legend.width = 1,
                    ylim = NULL, xlim = NULL, zlim = NULL,
                    add.legend = T,legend.only=F,
                    vline = NULL, col_vline = "black",
                    hline = NULL, col_hline = "black",
                    cex.lab = 2,cex.main = 2, myCol = NULL,
                    includeMarginals = F,
                    marginalJitterSD_x = 0.01,
                    marginalJitterSD_y = 0.01,
                    openBrowser = F){
  if(openBrowser){browser()}
  library(fields); library(akima)
  s_ <- interp(x=x, y=y, z=z,
               xo=seq(min(x),max(x),length=N),
               yo=seq(min(y),max(y),length=N),duplicate="mean")
  if(is.null(xlim)){xlim = c(summary( s_$x ))[c(1,6)]}
  if(is.null(ylim)){ylim = c(summary( s_$y ))[c(1,6)]}
  imageFxn <- image.plot
  if(add.legend == F){imageFxn <- image}
  if(!grepl(useLog,pattern="z")){
    imageFxn(s_, xlab = xlab, ylab = ylab, log = useLog, cex.lab = cex.lab , main = main, cex.main = cex.main,
             col = myCol,  xlim = xlim, ylim = ylim,
             legend.width = legend.width,
             horizontal = horizontal, yaxt = yaxt,
             zlim = zlim, legend.only = legend.only)
  }
  if(grepl(useLog,pattern="z")){
    useLog <- gsub(useLog,pattern="z",replace ="")
    zTicks <- summary( c(s_$z ))
    ep_ <- 0.001
    zTicks[zTicks<ep_] <- ep_
    zTicks <- exp(seq(log(min(zTicks)),log(max(zTicks)), length.out = 10))
    zTicks <- round(zTicks, abs(min(log(zTicks,base=10))))
    s_$z[s_$z<ep_] <- ep_
    imageFxn(s_$x,s_$y,log(s_$z),yaxt = yaxt,
             axis.args = list(at=log(zTicks),labels = zTicks), main = main, cex.main = cex.main,
             xlab = xlab, ylab = ylab,log = useLog, cex.lab = cex.lab,
             xlim = xlim,
             ylim = ylim,,horizontal=horizontal,
             col = myCol,legend.width=legend.width,
             zlim = zlim,legend.only = legend.only)
  }
  if(!is.null(vline)){ abline(v=vline,lwd=10,col=col_vline) }
  if(!is.null(hline)){ abline(h=hline,lwd=10,col=col_hline) }
  
  if(includeMarginals == T){
    points(x+rnorm(length(y),sd=marginalJitterSD_x*sd(x)), rep(ylim[1]*1.1,length(y)),pch="|",col="darkgray")
    points(rep(xlim[1]*1.1,length(x)), y+rnorm(length(y),sd=sd(y)*marginalJitterSD_y),pch="-",col="darkgray")
  }
}

#' Implements...
#'
#' @usage
#'
#' colSummmary(...)
#'
#' @param x Description
#'
#' @return `z` Description
#' @export
#'
#' @details `image2` implements...
#'
#' @examples
#'
#' # Perform analysis
#' colSummmary()
#'
#' @export
#'
#' @md
#' 
colSummmary <- function(x){ 
  apply(x,2,function(x_){
    x_f2n <- f2n(x_)
    if(!all(is.na(x_f2n))){ x_ <- mean(x_f2n, na.rm=T) }
    if(all(is.na(x_f2n))){ x_ <- names(table(x_)[which.max(table(x_))[1]]) }
    return( x_ ) })
}

#' Implements...
#'
#' @usage
#'
#' cols2numeric(...)
#'
#' @param x Description
#'
#' @return `z` Description
#' @export
#'
#' @details `cols2numeric` implements...
#'
#' @examples
#'
#' # Perform analysis
#' cols2numeric()
#'
#' @export
#'
#' @md

cols2numeric <- function(x){ 
  apply(x,2,function(x_){
    x_f2n <- f2n(x_)
    if(!all(is.na(x_f2n))){ x_ <- x_f2n }
    if(all(is.na(x_f2n))){ x_ <- x_ }
    return( x_ ) })
}

#' Implements...
#'
#' @usage
#'
#' MakeHeatMap(...)
#'
#' @param x Description
#'
#' @return `z` Description
#' @export
#'
#' @details `image2` implements...
#'
#' @examples
#'
#' # Perform analysis
#' MakeHeatMap()
#'
#' @export
#'
#' @md

MakeHeatMap <- function(factor1, factor2, outcome, dat, lm_obj, pdf_path, 
                        extrap_factor1 = 1, extrap_factor2 = 1, 
                        useLog = "", OUTCOME_SCALER = 1, 
                        OutcomeTransformFxn = function(x){x},
                        openBrowser = F){ 
  print2(sprintf("MakeHeatMap: %s and %s", factor1, factor2))
  if(openBrowser){browser()}
  eval(parse(text = sprintf('
    dat_new <- expand.grid("%s" = seq((1/extrap_factor1)*min(f2n(dat[,factor1])),
                                      extrap_factor1*max(f2n(dat[,factor1])), 
                                      length.out = 50L),
                           "%s" = seq((1/extrap_factor2)*min(f2n(dat[,factor2])),
                                      extrap_factor2*max(f2n(dat[,factor2])), 
                                      length.out = 50L))
                           ', factor1, factor2) ) ) 
  dat_new <- cbind(dat_new,
                   as.data.frame(t(colSummmary(dat[,!colnames(dat) %in% c(factor1,factor2,outcome)]))))
  dat_new$Yhat <- OutcomeTransformFxn( c(predict(lm_obj, newdata = dat_new)) ) 
  dat_new$Yhat <- dat_new$Yhat * OUTCOME_SCALER
  #hist(dat_new$Yhat)
  
  pdf( pdf_path )
  {
    par(mar=c(5,5,3,1), mfrow = c(1,1))
    xlim__ <- c(summary(dat_new[,factor1])[c(1,6)])
    ylim__ <- c(summary(dat_new[,factor2])[c(1,6)])
    zlim__ <- c(summary(dat_new$Yhat)[c(1,6)])
    myColScheme <- viridis::plasma(15,alpha = 0.9)
    heatMap(x = dat_new[,factor1],
            y = dat_new[,factor2],
            z = dat_new$Yhat,
            N = 50, myCol  = myColScheme,
            xlim = xlim__,  ylim = ylim__, zlim = zlim__, 
            main = "", 
            add.legend = T, horizontal = F,legend.width = 1,
            openBrowser = F,
            xlab = names(factor1), ylab = names(factor2),
            #yaxt ="n", useLog="xyz",
            useLog = useLog,
            includeMarginals = F)
    # mtext(text = expression("Entropy: Concentrated"%<->%"Dispersed"), side = 2,line = 3,cex=2)
  }
  dev.off()
}