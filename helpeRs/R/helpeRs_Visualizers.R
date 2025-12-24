#' Plot a matrix as an image with intuitive orientation
#'
#' A wrapper around \code{\link[graphics]{image}} that displays matrices with
#' the conventional orientation (first row at top, first column at left). The
#' base \code{image()} function displays matrices transposed and flipped, which
#' can be confusing. This function corrects that behavior.
#'
#' @param mat Numeric matrix to display as a heat map image.
#' @param xaxt Optional character vector of labels for the x-axis (columns).
#'   If \code{NULL} (default), no x-axis labels are drawn.
#' @param yaxt Optional character vector of labels for the y-axis (rows).
#'   If \code{NULL} (default), no y-axis labels are drawn.
#' @param col Optional vector of colors for the heat map. If \code{NULL},
#'   uses the default \code{image()} color palette.
#' @param main Optional character string for the plot title.
#' @param scale_vec Numeric vector of length 2 controlling the placement scaling
#'   of tick labels on the x and y axes. Default is \code{c(1, 1.04)}.
#' @param cex.axis Numeric value for character expansion of axis tick labels.
#'   Default is 1.
#'
#' @return Invisibly returns \code{NULL}. Called for its side effect of producing
#'   a plot.
#'
#' @seealso \code{\link{heatmap2}} for a higher-level heat map function,
#'   \code{\link{heatMap}} for interpolated heat maps from scattered data
#'
#' @examples
#' # Create a simple matrix
#' mat <- matrix(1:12, nrow = 3, ncol = 4)
#' rownames(mat) <- c("A", "B", "C")
#' colnames(mat) <- c("W", "X", "Y", "Z")
#'
#' # Plot with row/column labels
#' image2(mat, xaxt = colnames(mat), yaxt = rownames(mat), main = "Example")
#'
#' @export
image2 = function(mat,xaxt=NULL,yaxt = NULL,col=NULL,main=NULL,scale_vec=c(1,1.04),cex.axis = 1){
  # pretty plotting of 2D data
  image((t(mat)[,nrow(mat):1]),
        axes = F, main = main,xaxs = "i",cex.main = 2)
  if(!is.null(xaxt)){ axis(1, at = 0:(ncol(mat)-1)/ncol(mat)*scale_vec[1], tick=F,labels = (xaxt),cex.axis = cex.axis,las = 1)  }
  if(!is.null(yaxt)){ axis(2, at = 0:(nrow(mat)-1)/nrow(mat)*scale_vec[2], tick=F,labels = rev(yaxt),cex.axis = cex.axis,las = 2)  }
}

#' Create an interpolated heat map from scattered data
#'
#' Interpolates irregularly-spaced \code{(x, y, z)} observations onto a regular
#' grid using the \pkg{akima} package and visualizes the result using
#' \code{\link[fields]{image.plot}} from the \pkg{fields} package. This is useful
#' for visualizing relationships in data where observations are not on a regular
#' grid.
#'
#' @param x,y,z Numeric vectors of equal length defining the coordinates and
#'   values to interpolate. \code{x} and \code{y} are the spatial coordinates,
#'   \code{z} is the value at each location.
#' @param main Character string for the plot title. Default is empty.
#' @param N Integer specifying the number of grid cells in each direction for
#'   interpolation. Higher values produce smoother plots but take longer.
#' @param yaxt Optional character vector of labels for the y-axis. If \code{NULL}
#'   (default), numeric axis labels are used.
#' @param xlab,ylab Character strings for axis labels. Default is empty.
#' @param horizontal Logical; if \code{TRUE}, draw the color legend horizontally
#'   below the plot. Default is \code{FALSE} (vertical legend on right).
#' @param useLog Character string specifying which axes to log-transform.
#'   Can include \code{"x"}, \code{"y"}, and/or \code{"z"} (e.g., \code{"xyz"}
#'   for all axes). Default is empty (no log transformation).
#' @param legend.width Numeric value controlling the width of the color legend.
#'   Default is 1.
#' @param xlim,ylim,zlim Numeric vectors of length 2 giving the plot limits for
#'   each axis. If \code{NULL} (default), limits are computed from the data.
#' @param add.legend Logical; if \code{FALSE}, the color legend is suppressed.
#'   Default is \code{TRUE}.
#' @param legend.only Logical; if \code{TRUE}, draw only the legend without the
#'   main plot. Useful for creating custom layouts. Default is \code{FALSE}.
#' @param vline,hline Numeric values specifying positions of vertical or
#'   horizontal reference lines. Default is \code{NULL} (no lines).
#' @param col_vline,col_hline Colors for the reference lines. Default is
#'   \code{"black"}.
#' @param cex.lab,cex.main Numeric values for character expansion of axis labels
#'   and title. Default is 2 for both.
#' @param myCol Optional color palette vector. If \code{NULL}, uses the default
#'   \code{image.plot} palette.
#' @param includeMarginals Logical; if \code{TRUE}, adds 1D marginal rug plots
#'   showing the distribution of x and y values. Default is \code{FALSE}.
#' @param marginalJitterSD_x,marginalJitterSD_y Numeric values controlling the
#'   amount of jitter applied to marginal points, as a fraction of the standard
#'   deviation. Default is 0.01.
#' @param openBrowser Logical; if \code{TRUE}, enters debug mode via
#'   \code{browser()} for interactive inspection. Default is \code{FALSE}.
#'
#' @return Invisibly returns \code{NULL}. Called for its side effect of producing
#'   a plot.
#'
#' @seealso \code{\link{heatmap2}} for plotting matrices directly,
#'   \code{\link{MakeHeatMap}} for model-based prediction heat maps,
#'   \code{\link[fields]{image.plot}} for the underlying plotting function
#'
#' @examples
#' \dontrun{
#' # Create some scattered data
#' set.seed(42)
#' x <- runif(100, 0, 10)
#' y <- runif(100, 0, 10)
#' z <- sin(x) * cos(y) + rnorm(100, sd = 0.1)
#'
#' # Plot as interpolated heat map
#' heatMap(x, y, z, N = 50, main = "Example Heat Map",
#'         xlab = "X Variable", ylab = "Y Variable")
#' }
#'
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

  if(!grepl(useLog,pattern="z")){
    if(add.legend == TRUE){
      # Use image.plot with full parameter set
      image.plot(s_, xlab = xlab, ylab = ylab, log = useLog, cex.lab = cex.lab, main = main, cex.main = cex.main,
                 col = myCol, xlim = xlim, ylim = ylim,
                 legend.width = legend.width,
                 horizontal = horizontal,
                 zlim = zlim, legend.only = legend.only)
    } else {
      # Use base image with compatible parameters only
      image(s_, xlab = xlab, ylab = ylab, log = useLog, cex.lab = cex.lab, main = main, cex.main = cex.main,
            col = myCol, xlim = xlim, ylim = ylim)
    }
  }
  if(grepl(useLog,pattern="z")){
    useLog <- gsub(useLog,pattern="z",replace ="")
    zTicks <- summary( c(s_$z ))
    ep_ <- 0.001
    zTicks[zTicks<ep_] <- ep_
    zTicks <- exp(seq(log(min(zTicks)),log(max(zTicks)), length.out = 10))
    zTicks <- round(zTicks, abs(min(log(zTicks,base=10))))
    s_$z[s_$z<ep_] <- ep_
    if(add.legend == TRUE){
      # Use image.plot with full parameter set
      image.plot(s_$x,s_$y,log(s_$z),
                 axis.args = list(at=log(zTicks),labels = zTicks), main = main, cex.main = cex.main,
                 xlab = xlab, ylab = ylab,log = useLog, cex.lab = cex.lab,
                 xlim = xlim, ylim = ylim, horizontal=horizontal,
                 col = myCol,legend.width=legend.width,
                 zlim = zlim,legend.only = legend.only)
    } else {
      # Use base image with compatible parameters only
      image(s_$x,s_$y,log(s_$z), main = main, cex.main = cex.main,
            xlab = xlab, ylab = ylab, log = useLog, cex.lab = cex.lab,
            xlim = xlim, ylim = ylim, col = myCol)
    }
  }
  if(!is.null(vline)){ abline(v=vline,lwd=10,col=col_vline) }
  if(!is.null(hline)){ abline(h=hline,lwd=10,col=col_hline) }
  
  if(includeMarginals == T){
    points(x+rnorm(length(y),sd=marginalJitterSD_x*sd(x)), rep(ylim[1]*1.1,length(y)),pch="|",col="darkgray")
    points(rep(xlim[1]*1.1,length(x)), y+rnorm(length(y),sd=sd(y)*marginalJitterSD_y),pch="-",col="darkgray")
  }
}

#' Quick heat map visualization of a matrix
#'
#' A convenience wrapper around \code{\link{heatMap}} for fast visualization of
#' matrix-like objects. Can use either base R graphics or \pkg{ggplot2} for a
#' more polished appearance.
#'
#' When \code{use_gg = TRUE}, the function creates a tile plot using
#' \code{\link[ggplot2]{geom_tile}} with a white-to-steelblue gradient. Marginal
#' histograms can be added using \pkg{ggExtra} if available.
#'
#' @param mat Numeric matrix or data frame to visualize. Will be coerced to a
#'   matrix if necessary.
#' @param row_labels Optional character vector of labels for rows. If \code{NULL}
#'   (default), uses \code{rownames(mat)}.
#' @param col_labels Optional character vector of labels for columns. If
#'   \code{NULL} (default), uses \code{colnames(mat)}.
#' @param use_gg Logical; if \code{TRUE}, creates a \pkg{ggplot2} plot instead
#'   of using base graphics. Requires the \pkg{ggplot2} package. Default is
#'   \code{FALSE}.
#' @param scale Logical; if \code{TRUE}, standardizes values (subtracts mean,
#'   divides by SD) before plotting. Default is \code{FALSE}.
#' @param log Logical; if \code{TRUE}, applies natural log transformation to
#'   values before plotting. Default is \code{FALSE}.
#' @param includeMarginals Logical; if \code{TRUE} and \code{use_gg = TRUE},
#'   adds marginal histograms using \pkg{ggExtra}. For base graphics, adds
#'   marginal rug plots. Default is \code{FALSE}.
#' @param ... Additional arguments passed to \code{\link{heatMap}} when
#'   \code{use_gg = FALSE}.
#'
#' @return When \code{use_gg = TRUE}, invisibly returns the ggplot object.
#'   Otherwise, invisibly returns \code{NULL}. Called for its side effect of
#'   producing a plot.
#'
#' @seealso \code{\link{heatMap}} for the underlying scatter-to-grid
#'   interpolation, \code{\link{image2}} for simple matrix plotting
#'
#' @examples
#' # Create a correlation matrix
#' cor_mat <- cor(mtcars[, 1:5])
#'
#' # Quick base R plot
#' heatmap2(cor_mat, row_labels = colnames(cor_mat),
#'          col_labels = colnames(cor_mat))
#'
#' \dontrun{
#' # ggplot2 version
#' heatmap2(cor_mat, use_gg = TRUE)
#' }
#'
#' @export
heatmap2 <- function(mat, row_labels = NULL, col_labels = NULL,
                     use_gg = FALSE, scale = FALSE, log = FALSE,
                     includeMarginals = FALSE, ...){
  mat <- as.matrix(mat)
  if(scale){
    mat <- scale(mat)
  }
  if(log){
    mat <- log(mat)
  }

  if(use_gg){
    if(!requireNamespace("ggplot2", quietly = TRUE)){
      stop("Package 'ggplot2' is required for use_gg = TRUE")
    }
    df <- as.data.frame(as.table(mat))
    df$Var1 <- factor(df$Var1,
                      levels = rev(seq_len(nrow(mat))),
                      labels = rev(if(is.null(row_labels)) rownames(mat) else row_labels))
    df$Var2 <- factor(df$Var2,
                      levels = seq_len(ncol(mat)),
                      labels = if(is.null(col_labels)) colnames(mat) else col_labels)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = Var2, y = Var1, fill = Freq)) +
      ggplot2::geom_tile() +
      ggplot2::scale_y_discrete(limits = rev(levels(df$Var1))) +
      ggplot2::scale_fill_gradient(low = "white", high = "steelblue") +
      ggplot2::labs(x = NULL, y = NULL)
    if(includeMarginals && requireNamespace("ggExtra", quietly = TRUE)){
      p <- ggExtra::ggMarginal(p, type = "histogram")
    }
    print(p)
    return(invisible(p))
  }

  heatMap(x = rep(seq_len(ncol(mat)), each = nrow(mat)),
          y = rep(seq_len(nrow(mat)), times = ncol(mat)),
          z = as.vector(mat),
          N = max(ncol(mat), nrow(mat)),
          includeMarginals = includeMarginals,
          ...)
  # Add custom axis labels using actual data coordinates
  if(!is.null(col_labels)){
    axis(1, at = seq_len(ncol(mat)), labels = col_labels, las = 2)
  }
  if(!is.null(row_labels)){
    axis(2, at = seq_len(nrow(mat)), labels = row_labels, las = 1)
  }
  invisible(NULL)
}

#' Summarize each column of a data frame
#'
#' Computes a single summary value for each column of a data frame. Numeric
#' columns are summarized by their mean, while non-numeric columns are
#' summarized by their mode (most frequent value). This is primarily used when
#' constructing prediction grids for visualization, where you need representative
#' values for variables not being varied.
#'
#' The function uses \code{\link{f2n}} internally to attempt numeric conversion,
#' so factor columns with numeric-looking levels will be treated as numeric.
#'
#' @param x A data frame to summarize.
#'
#' @return A named vector with one element per column of \code{x}. Names
#'   correspond to column names. Numeric summaries are means; non-numeric
#'   summaries are the most frequent value.
#'
#' @seealso \code{\link{cols2numeric}} for converting columns to numeric,
#'   \code{\link{MakeHeatMap}} which uses this function internally
#'
#' @examples
#' # Summarize mtcars
#' colSummmary(mtcars[, 1:4])
#'
#' # Mixed numeric and character columns
#' df <- data.frame(
#'   x = c(1, 2, 3, 4),
#'   y = c("a", "b", "a", "a")
#' )
#' colSummmary(df)  # Returns mean of x and mode of y
#'
#' @export
colSummmary <- function(x){ 
  apply(x,2,function(x_){
    x_f2n <- f2n(x_)
    if(!all(is.na(x_f2n))){ x_ <- mean(x_f2n, na.rm=T) }
    if(all(is.na(x_f2n))){ x_ <- names(table(x_)[which.max(table(x_))[1]]) }
    return( x_ ) })
}

#' Convert data frame columns to numeric when possible
#'
#' Attempts to coerce each column of a data frame to numeric using
#' \code{\link{f2n}}. Columns that cannot be coerced (i.e., result in all
#' \code{NA} values) are left unchanged. This is useful for cleaning data
#' where numeric values may be stored as factors or character strings.
#'
#' @param x A data frame to process.
#'
#' @return A data frame with the same dimensions as \code{x}. Columns that
#'   could be converted to numeric are returned as numeric; others retain
#'   their original type.
#'
#' @seealso \code{\link{f2n}} for converting individual vectors,
#'   \code{\link{colSummmary}} for summarizing columns
#'
#' @examples
#' # Data frame with mixed types
#' df <- data.frame(
#'   a = factor(c("1.5", "2.5", "3.5")),
#'   b = c("x", "y", "z"),
#'   c = c(1, 2, 3),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Convert what can be converted
#' df_numeric <- cols2numeric(df)
#' sapply(df_numeric, class)
#' # a becomes numeric, b stays character, c stays numeric
#'
#' @export
cols2numeric <- function(x){ 
  apply(x,2,function(x_){
    x_f2n <- f2n(x_)
    if(!all(is.na(x_f2n))){ x_ <- x_f2n }
    if(all(is.na(x_f2n))){ x_ <- x_ }
    return( x_ ) })
}

#' Visualize two-way predictor effects as a heat map
#'
#' Creates a prediction surface showing how the expected outcome varies across
#' a grid of two predictor variables, holding all other predictors at their
#' mean (numeric) or mode (categorical) values. The result is saved as a PDF
#' file.
#'
#' The function uses \code{\link{colSummmary}} to compute representative values
#' for predictors not being varied, then generates predictions across a 50x50
#' grid spanning the ranges of the two focal predictors. The predictions are
#' visualized using \code{\link{heatMap}} with the \pkg{viridis} color palette.
#'
#' @param factor1,factor2 Character strings giving the names of the two
#'   predictor variables to vary. These must be columns in \code{dat}.
#' @param outcome Character string giving the name of the outcome variable.
#'   Used only for excluding from the summary computation.
#' @param dat Data frame containing the data used to fit the model. Used to
#'   determine variable ranges and compute summary values.
#' @param lm_obj Fitted linear model object (from \code{lm()} or similar) used
#'   for generating predictions.
#' @param pdf_path Character string giving the file path where the PDF plot
#'   will be saved.
#' @param extrap_factor1,extrap_factor2 Numeric multipliers controlling how far
#'   beyond the observed data range the prediction grid extends. Values > 1
#'   extrapolate beyond the data; values < 1 restrict to a subset of the range.
#'   Default is 1 (exact data range).
#' @param useLog Character string specifying which axes to log-transform.
#'   Can include \code{"x"}, \code{"y"}, and/or \code{"z"}. Default is empty
#'   (no transformation).
#' @param OUTCOME_SCALER Numeric value by which predictions are multiplied
#'   before plotting. Useful for unit conversions. Default is 1.
#' @param OutcomeTransformFxn Function applied to predictions before scaling.
#'   Default is the identity function \code{function(x) x}.
#' @param openBrowser Logical; if \code{TRUE}, pauses execution via
#'   \code{browser()} for interactive debugging. Default is \code{FALSE}.
#'
#' @return Invisibly returns \code{NULL}. The function is called for its side
#'   effect of writing a PDF file to \code{pdf_path}.
#'
#' @seealso \code{\link{heatMap}} for the underlying plotting function,
#'   \code{\link{colSummmary}} for computing representative predictor values
#'
#' @examples
#' \dontrun{
#' # Fit a model
#' fit <- lm(mpg ~ wt + hp + cyl, data = mtcars)
#'
#' # Create heat map of mpg as function of wt and hp
#' MakeHeatMap(
#'   factor1 = "wt",
#'   factor2 = "hp",
#'   outcome = "mpg",
#'   dat = mtcars,
#'   lm_obj = fit,
#'   pdf_path = "mpg_heatmap.pdf"
#' )
#' }
#'
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