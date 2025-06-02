#' Plot a matrix as an image with optional axis labels
#'
#' This is a thin wrapper around [graphics::image()] that flips the matrix so
#' the first row is shown at the top of the plot.  Axis tick labels can be
#' supplied via `xaxt` and `yaxt`.
#'
#' @param mat Matrix to display.
#' @param xaxt,yaxt Optional character vectors containing the x and y axis
#'   labels.  If `NULL` no axis ticks are drawn.
#' @param col Optional vector of colours passed to [graphics::image()].
#' @param main Optional plot title.
#' @param scale_vec Numeric vector controlling the placement of tick labels on
#'   the x and y axes.
#' @param cex.axis Character expansion for axis tick labels.
#'
#' @return Invisibly returns `NULL`. The function is called for its side effect
#'   of producing a plot.
#' @export

image2 = function(mat,xaxt=NULL,yaxt = NULL,col=NULL,main=NULL,scale_vec=c(1,1.04),cex.axis = 1){
  # pretty plotting of 2D data
  image((t(mat)[,nrow(mat):1]),
        axes = F, main = main,xaxs = "i",cex.main = 2)
  if(!is.null(xaxt)){ axis(1, at = 0:(ncol(mat)-1)/ncol(mat)*scale_vec[1], tick=F,labels = (xaxt),cex.axis = cex.axis,las = 1)  }
  if(!is.null(yaxt)){ axis(2, at = 0:(nrow(mat)-1)/nrow(mat)*scale_vec[2], tick=F,labels = rev(yaxt),cex.axis = cex.axis,las = 2)  }
}

#' Interpolate scattered data and draw a heat map
#'
#' This helper uses the `fields` and `akima` packages to interpolate scattered
#' `(x, y, z)` observations onto a regular grid and then visualises the result
#' with `image.plot`.
#'
#' @param x,y,z Numeric vectors defining the coordinates and values to
#'   interpolate.
#' @param main Character string used as the plot title.
#' @param N Number of grid cells in each direction passed to `interp`.
#' @param yaxt Optional labels for the y axis; if `NULL` (default) labels are
#'   omitted.
#' @param xlab,ylab Axis labels passed to the plotting function.
#' @param horizontal Logical; if `TRUE` draw the legend horizontally.
#' @param useLog Character string specifying axes to be log-transformed
#'   (e.g. `"xyz"`).
#' @param legend.width Width of the colour legend.
#' @param ylim,xlim,zlim Numeric vectors giving plot limits.
#' @param add.legend Logical; if `FALSE` the legend is suppressed.
#' @param legend.only Logical; if `TRUE` draw only the legend.
#' @param vline,hline Optional positions of vertical or horizontal reference
#'   lines.
#' @param col_vline,col_hline Colours for the reference lines.
#' @param cex.lab,cex.main Character expansion for axis labels and title.
#' @param myCol Optional colour palette.
#' @param includeMarginals Logical; add 1D marginal rugs if `TRUE`.
#' @param marginalJitterSD_x,marginalJitterSD_y Numeric jitter amounts used when
#'   drawing marginals.
#' @param openBrowser Logical; if `TRUE` enters debug mode via `browser()`.
#'
#' @return Invisibly returns `NULL`.  Called for its plotting side effect.
#' @export

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

#' Summarise each column of a data frame
#'
#' Numeric columns are replaced by their mean while non-numeric columns are
#' replaced by their most common value.  This is mainly used when constructing
#' prediction grids for plotting.
#'
#' @param x A data frame.
#' @return A vector of summary values with one entry per column of `x`.
#' @export
#' 
colSummmary <- function(x){ 
  apply(x,2,function(x_){
    x_f2n <- f2n(x_)
    if(!all(is.na(x_f2n))){ x_ <- mean(x_f2n, na.rm=T) }
    if(all(is.na(x_f2n))){ x_ <- names(table(x_)[which.max(table(x_))[1]]) }
    return( x_ ) })
}

#' Convert data frame columns to numeric when possible
#'
#' Attempts to coerce each column of a data frame to numeric.  Columns that
#' cannot be coerced are left unchanged.
#'
#' @param x A data frame.
#' @return A data frame with the same shape as `x`.
#' @export

cols2numeric <- function(x){ 
  apply(x,2,function(x_){
    x_f2n <- f2n(x_)
    if(!all(is.na(x_f2n))){ x_ <- x_f2n }
    if(all(is.na(x_f2n))){ x_ <- x_ }
    return( x_ ) })
}

#' Visualise a two-way predictor effect as a heat map
#'
#' Creates a grid over the ranges of two predictor variables and uses a fitted
#' model to predict the outcome on that grid.  The predictions are then plotted
#' with [heatMap()] and saved to `pdf_path`.
#'
#' @param factor1,factor2 Names of the predictor variables to vary.
#' @param outcome Name of the outcome variable.
#' @param dat Data frame used to fit the model.
#' @param lm_obj Fitted linear model object.
#' @param pdf_path File path to save the resulting heat map.
#' @param extrap_factor1,extrap_factor2 Multipliers controlling how far beyond
#'   the data range the grid should extend.
#' @param useLog Character string specifying axes to log-transform.
#' @param OUTCOME_SCALER Multiplicative factor applied to predicted values.
#' @param OutcomeTransformFxn Function applied to predictions before plotting.
#' @param openBrowser Logical; if `TRUE` pauses execution via `browser()` for
#'   interactive inspection.
#'
#' @return Invisibly returns `NULL`. The heat map is written to `pdf_path`.
#' @export

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