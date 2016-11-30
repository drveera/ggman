#' Add labels to ggman Plot
#'
#' Adds user provided labels to the ggman Manhattan plot
#'
#' @param ggmanPlot A ggman plot object
#' @param labelDfm A data frame containing the snps and the labels
#' @param snp Name of the column containing the markers;
#' @param label Name of the column containing the labels
#' @param type type of label; either "label" or "text"
#' @param colour Colour of the label
#' @param size of the label
#'
#' @example
#'
#' @export
ggmanLabel <- function(ggmanPlot,labelDfm,
                      snp = "snp",
                      label = "label",
                      type = "label",
                      colour  = "black",
                      size = 2) {
    dfm <- ggmanPlot[[1]]
    labelDfm$snp <- labelDfm[,snp]
    labelDfm$label <- labelDfm[,label]
    dfm.sub <- merge(dfm,labelDfm, by = "snp")
    if (type == "label"){
        ggmanPlot +
            geom_label_repel(data = dfm.sub, aes(label = label), colour = colour, size = size) +
            geom_point(data = dfm.sub,shape = 5)
    } else if (type == "text"){
        ggmanPlot +
            geom_text_repel(data = dfm.sub, aes(label = label), colour = colour, size = size) +
            geom_point(data = dfm.sub,shape = 5)
    }
    
}
