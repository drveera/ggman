#' Add labels to ggman Plot
#'
#' Label points in the ggman plot.
#'
#' 
#' 
#'
#' This function adds a layer of textual annotation using \code{\link[ggrepel]{geom_label_repel }}(if type = "label") or \code{\link[ggrepel]{geom_text_repel}} (if type = "text")
#'
#' @param ggmanPlot A ggplot layer of class 'ggman'; see \code{\link{ggman}}
#' @param labelDfm A data frame object with the SNPs and the labels;  
#' @param snp Name of the column containing the markers;
#' @param label Name of the column containing the labels; It can be the snp column itself.
#' @param type type of label; either "label" or "text"; see details section
#' @param ... other arguments applicable to \code{\link[ggrepel]{geom_text_repel}} or \code{\link[ggrepel]{geom_label_repel}}
#'
#'@import ggplot2
#' 
#'@import ggrepel
#' 
#' @examples
#'
#' #label type annotations
#' ggmanLabel(p1, labelDfm = toy.gwas.sig, snp = "snp", label = "snp")
#' 
#'
#' #text type annotations
#' ggmanLabel(p1, labelDfm = toy.gwas.sig, snp = "snp", label = "snp", type = "text")
#'
#' #change the colour and size of labels
#' ggmanLabel(p1, labelDfm = toy.gwas.sig, snp = "snp", label = "snp", colour = "black", size = 2)
#' 
#'
#' @export
ggmanLabel <- function(ggmanPlot,labelDfm,
                      snp = "snp",
                      label = "label",
                      type = "label", ...) {

    ##input checks
    environment(check.input.ggmanLabel) = environment()
    check.input.ggmanLabel()
    
    dfm <- ggmanPlot[[1]]
    labelDfm$snp <- labelDfm[,snp]
    labelDfm$label <- labelDfm[,label]
    dfm.sub <- merge(dfm,labelDfm, by = "snp")
    if (type == "label"){
        ggmanPlot +
            geom_label_repel(data = dfm.sub, aes(label = label),...) +
            geom_point(data = dfm.sub,shape = 5)
    } else if (type == "text"){
        ggmanPlot +
            geom_text_repel(data = dfm.sub, aes(label = label), ...) +
            geom_point(data = dfm.sub,shape = 5)
    }
    
}
