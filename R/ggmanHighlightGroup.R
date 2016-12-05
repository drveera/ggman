#' Highlight groups of points in Manhattan plot
#'
#' Highlights groups of points in the ggman plot and add a legend.
#'
#' @param ggmanPlot A ggman plot
#' @param highlightDfm A data frame with the snps to highlight
#' @param snp Name of the snp column
#' @param group Name of the grouping column
#' @param legend.title Title of the legend.
#' @param legend.remove If TRUE, legend will be removed.
#' @param ... other arguments passed to \code{\link[ggplot2]{geom_point}}
#' 
#'
#' @return A manhattan plot with highlighted markers
#'
#' @examples
#'
#' @export
ggmanHighlightGroup <- function(ggmanPlot,
                           highlightDfm,
                           snp = "snp",
                           group = "group",
                           legend.title = "legend",
                           legend.remove = FALSE,
                           ...){
    ##input checks
    environment(check.input.ggmanHighlightGroup) <- environment()
    check.input.ggmanHighlightGroup()
    
    dfm <- ggmanPlot[[1]]
    highlightDfm$snp <- highlightDfm[,snp]
    highlightDfm$group <- as.factor(highlightDfm[,group])

    dfm.sub <- merge(dfm,highlightDfm, by="snp")
    
    #dfm <- dfm[dfm$snp %in% highlight,]
    if(nrow(dfm.sub) == 0){
        stop("None of the markers in highlight input is present in the Manhattan plot layer")
    }

    p1 <- ggmanPlot +
        scale_colour_grey(start = 0.5,end = 0.6) +
        geom_point(data = dfm.sub,aes(fill = as.factor(group)),shape=21,
                   colour = alpha("black",0),...) +
        scale_fill_discrete(name = legend.title)
    if(legend.remove){
        p1 + guides(fill = FALSE)
    } else {
        p1        
    }
        
}
