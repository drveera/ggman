#' A ggPlot wrapper for Manhattan Plot
#'
#' Creates a ggPlot layer for Manhattan Plot
#'
#' @param gwas A data frame object with the gwas results
#' @param snp Name of the column containing the markers to plot
#' @param bp Name of the column containing the basepair positions
#' @param chrom Name of the column containing the chromosome information
#' @param pvalue Name of the column containing the p values to plot
#' @param sigLine Threshold for the genome wide significant line in -log10 scale
#' @param lineColor colour of the genomewide line
#' @param pointSize Size of the points in the plot supplied to geom_point
#' @param ymin Starting point of y axis
#' @param ymax Ending point of y axis
#' @param logTransform Whether to log transform the p values
#' @param xlabel x axis label
#' @param ylabel y  axis label
#' @param title plot title
#'
#' @importFrom gtools mixedorder
#' 
#' @return If unassigned to a variable, returns a manhattan plot; If assigned to a variable, stored as a ggplot layer
#'
#' @examples
#'
#' @export
ggmanInvert <- function(gwas,
                        snp = "snp", bp = "bp", chrom = "chrom", pvalue = "pvalue",
                        effect = "or", method = "or",
                  sigLine = 8,
                  lineColor = "red",
                  pointSize = 0.1,
                  ymin = NA, ymax = NA,             
                  xlabel = "chromosome", ylabel = "-log10(P value)", title = "Manhattan Plot") {
    library(ggplot2)
    library(ggrepel)
    gwas <- as.data.frame(gwas)
    chrom <- gwas[,chrom]
    bp <- as.numeric(as.character(gwas[,bp]))
    pvalue <- as.numeric(as.character(gwas[,pvalue]))
    snp <- gwas[,snp]
    dfm <- data.frame(chrom,bp,snp,pvalue)
    dfm$chrom <- as.character(dfm$chrom)

    #format p values
    if(method == "or"){
        dfm$or <- as.numeric(as.character(gwas[,effect]))
        dfm$sign <- with(dfm, replace(pvalue,or > 1, 1))
        dfm$sign <- with(dfm, replace(sign, sign != 1, -1))
    }
    if(method == "beta"){
        dfm$beta <- as.numeric(as.character(gwas[,effect]))
        dfm$sign <- with(dfm, replace(beta, beta > 0, 1))
        dfm$sign <- with(dfm, replace(sign, sign != 1, -1))       
    }

    dfm$marker <- with(dfm, -log10(pvalue) * sign)

    ##yaxis limits

    if(is.na(ymax)){
        ymax <- max(dfm$marker) + 1
    }

    if(is.na(ymin)){
        ymin <- ymax * -1
    }


    ##add index
    #library(gtools)
    dfm <- dfm[order(dfm$bp),]
    dfm <- dfm[mixedorder(dfm$chrom),]
    dfm$index <- 1:nrow(dfm)

    ##find the number of chromosomes
    chrtable <- data.frame(table(dfm$chrom))
    chrtable$Var1 <- as.character(chrtable$Var1)
    chrtable <- chrtable[mixedorder(chrtable$Var1),]
    ##group odd and even chromosomes
    oddchrom <- as.character(chrtable$Var1[seq(1,nrow(chrtable),2)])
    dfm$chrom_alt <- replace(dfm$chrom, dfm$chrom %in% oddchrom, 0)
    dfm$chrom_alt <- replace(dfm$chrom_alt, dfm$chrom_alt != 0,1)

    ##create x axis tick points
    dfmsplit <- split(dfm, dfm$chrom)
    xbreaks <- sapply(dfmsplit,function(x) x$index[length(x$index)/2])

        ggplot(dfm, aes(index,marker, colour = as.factor(chrom_alt))) +
                geom_point(size=pointSize) +
                scale_x_continuous(breaks = xbreaks, labels = names(xbreaks), expand = c(0,0)) +
            geom_hline(aes(yintercept= as.numeric(sigLine)),colour = "red", size = 0.25) +
            geom_hline(aes(yintercept= as.numeric(sigLine)*-1),colour = "red", size = 0.25) +
            geom_hline(aes(yintercept= 0),colour = "white", size = 0.25) +
                scale_y_continuous(expand = c(0,0), limits = c(ymin,ymax))+
            guides(colour = FALSE) + labs(x = xlabel, y = ylabel, title = title)
}
