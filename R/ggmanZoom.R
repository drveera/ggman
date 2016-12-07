#' ggmanZoom
#'
#' Zoom in to a specific region of the Manhattan Plot
#'
#' @param ggmanPlot A ggplot object of class 'ggman'
#' @param chromosome Chromosome identifier
#' @param start.position Starting basepair position
#' @param end.position Ending basepair position
#' @param xlabel X axis label
#' @param ylabel Y axis label
#' @param title Plot title
#' @param highlight.group Name of the column containing grouping variable in the parent data.frame that was used to create main ggplot layer.
#' @param legend.title title for legend; default is 'legend'
#' @param legend.remove if TRUE, the legend will be removed; default is FALSE
#' @param ... other arguments to pass to \code{\link{geom_point}}
#'
#' @examples
#'
#' #specific chromosome
#' ggmanZoom(p1, chromosome = 1)
#'
#' #specific region
#' ggmanZoom(p1, chromosome = 1, start.position = 215388741, end.position = 238580695)
#'
#' #add highlights and legend
#' ggmanZoom(p1, chromosome = 1, start.position = 215388741, end.position = 238580695,
#'           highlight.group = "gene", legend.title = "Genes")
#' 
#'
#' @export
ggmanZoom <- function(
                      ggmanPlot,
                      chromosome,
                      start.position=NA,
                      end.position=NA,
                      xlabel = NA,
                      ylabel = NA,
                      title = NA,
                      highlight.group = NA,
                      legend.title = "legend",
                      legend.remove = FALSE,
                      ...
                      ){
    ##check inputs
    environment(check.input.ggmanZoom) <- environment()
    check.input.ggmanZoom()
    
    dfm <- ggmanPlot[[1]]
    if (is.na(start.position)){
        dfm.sub <- dfm[dfm$chrom == chromosome,]
    } else {
        dfm.sub <- dfm[dfm$bp >= start.position & dfm$bp <= end.position & dfm$chrom == chromosome,]
    }

    if(! is.na(highlight.group)){
        dfm.sub$group = dfm.sub[,highlight.group]
    }
    
    xtick1 <- min(dfm.sub$index)
    xtick1.label <- min(dfm.sub$bp)
    xtick3 <- max(dfm.sub$index)
    xtick3.label <- max(dfm.sub$bp)
    xtick2 <- dfm.sub$index[nrow(dfm.sub)/2]
    xtick2.label <- dfm.sub[dfm.sub$index == xtick2,]$bp
    xbreaks <- c(xtick1,xtick2,xtick3)
    xlabels <- c(xtick1.label,xtick2.label,xtick3.label)
    title <- "Regional association plot"
    if(is.na(xlabel)){
        if(is.na(start.position)){
            xlabel = paste0("Chromosome",chromosome)
        } else {
            xlabel = paste0("Chromosome",chromosome,":",start.position,"-",end.position)
        }
    }

    if(is.na(ylabel)){
        ylabel = expression(paste("-log" ["10"],"P Value"))
    }

    if(is.na(highlight.group)){
        p1 <- ggplot(dfm.sub, aes(index,marker)) + geom_point(...) +
        scale_x_continuous(breaks = xbreaks, labels = xlabels) +
        labs(x = xlabel, y = ylabel, title = title)    
    } else {
            p1 <- ggplot(dfm.sub, aes(index,marker, colour = as.factor(group))) + geom_point(...) +
        scale_x_continuous(breaks = xbreaks, labels = xlabels) +
        labs(x = xlabel, y = ylabel, title = title, colour = legend.title)
    }

    if(legend.remove){
        p1 + guides(colour = FALSE)
    } else {
        p1
    }

    
}
