#' Highlight markers in Manhattan plot
#'
#' Highlights a specific subset of markers in the ggman plot
#'
#' @param ggmanPlot A ggman plot
#' @param highlight A character vector of SNP ids to highlight
#' @param colour Colour of the highlight
#' @param pointSize Size of the highlight points
#'
#' @return A manhattan plot with highlighted markers
#'
#' @examples
#'
#' @export
ggmanHighlight <- function(ggmanPlot, highlight,
                           colour = "red",
                           pointSize = 0.1){
    dfm <- ggmanPlot[[1]]
    dfm <- dfm[dfm$snp %in% highlight,]
    ggmanPlot +
        scale_colour_grey(start = 0.5,end = 0.6) +
        geom_point(data = dfm,colour = colour, size = pointSize)
}
