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
                           colour = "green",
                           pointSize = 0.1){
    dfm <- ggmanPlot[[1]]
    dfm <- dfm[dfm$snp %in% highlight,]
    ggmanPlot +
        scale_colour_grey() +
        geom_point(data = dfm,colour = colour, size = pointSize)
}
