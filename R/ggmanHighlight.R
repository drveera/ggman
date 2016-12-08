#' Highlight points in Manhattan plot
#'
#' Highlights a specific subset of points in the Manhattan plot
#'
#' @param ggmanPlot A ggman plot of class 'ggman'
#' @param highlight A character vector of SNP ids to highlight
#' @param colour highlight colour
#' @param ... other arguments passed to \code{\link[ggplot2]{geom_point}}
#' 
#'
#' @return A manhattan plot with highlighted markers
#'
#' @examples
#' p1 <- ggman(toy.gwas, snp = "snp", bp = "bp", chrom = "chrom",
#' pvalue = "pvalue")
#' ggmanHighlight(p1, highlight = toy.highlights)
#'
#' @export
ggmanHighlight <- function(ggmanPlot, highlight,colour = "red", ...){
    ##input checks
    environment(check.input.ggmanHighlight) <- environment()
    check.input.ggmanHighlight()
    
    dfm <- ggmanPlot[[1]]
    dfm <- dfm[dfm$snp %in% highlight,]
    if(nrow(dfm) == 0){
        stop("None of the markers in the input is present in the Manhattan plot layer")
    }
    ggmanPlot +
        scale_colour_grey(start = 0.5,end = 0.6) +
        geom_point(data = dfm,colour= colour,...)
}
