#' Highlight points in Manhattan plot
#'
#' Highlights a specific subset of points in the ggman plot
#'
#' @param ggmanPlot A ggman plot
#' @param highlight A character vector of SNP ids to highlight
#' @param colour highlight colour
#' @param ... other arguments passed to \code{\link[ggplot2]{geom_pont}}
#' 
#'
#' @return A manhattan plot with highlighted markers
#'
#' @examples
#'
#' @export
ggmanHighlight <- function(ggmanPlot, highlight,colour = "red",
                           size = 0.1, ...){
    ##input checks
    environment(check.input.ggmanHighlight) <- environment()
    check.input.ggmanHighlight()
    
    dfm <- ggmanPlot[[1]]
    dfm <- dfm[dfm$snp %in% highlight,]
    if(nrow(dfm) == 0){
        stop("None of the markers in highlight input is present in the Manhattan plot layer")
    }
    ggmanPlot +
        scale_colour_grey(start = 0.5,end = 0.6) +
        geom_point(data = dfm,colour= colour, size = size,...)
}
