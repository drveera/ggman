#' A ggPlot wrapper for Manhattan Plot
#'
#' Creates a ggPlot layer for Manhattan Plot
#'
#' @param gwas A data frame object with the gwas results
#' @param clumps A clump object, if clumping is required
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
ggman <- function(gwas,
                  clumps = NA,
                  snp = "snp", bp = "bp", chrom = "chrom", pvalue = "pvalue",
                  sigLine = 8,
                  lineColor = "red",
                  pointSize = 0.1,
                  ymin = 0, ymax = 10,
                  logTransform = TRUE,
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

    if(logTransform){
        dfm$marker <- -log10(dfm$pvalue)
    } else {
        dfm$marker <- dfm$pvalue
    }


    if (!is.na(clumps)[1]){
        if (!any(class(clumps) == "ggclumps")) {
            cat("clumps argument takes an object of class ggclumps;see readClumps function")
            q()
        }
        clumpedSnps <- unlist(clumps)
        indexSnps <- names(clumps)
        clumpedSnps <- c(clumpedSnps, indexSnps )
        index <- dfm$index
        names(index) <- dfm$snp
        for (i in indexSnps){
            cat (i,":",index[i],"\n")
            index <- replace(index, names(index) %in% clumps[[which(names(clumps) == i)]], index[i])
        }
        dfm$index <- index
        dfm.sub <- dfm[dfm$snp %in% clumpedSnps,]
        dfm.index <- dfm[dfm$snp %in% indexSnps,]
        
        ggplot(dfm, aes(index,marker, colour = as.factor(chrom_alt))) +
            geom_point(size=pointSize) +
            scale_x_continuous(breaks = xbreaks, labels = names(xbreaks), expand = c(0,0)) +
            geom_hline(aes(yintercept= as.numeric(sigLine)),colour = "red", size = 0.25) +
            scale_y_continuous(expand = c(0,0), limits = c(ymin,ymax))  +
            guides(colour = FALSE) +
            scale_colour_grey() +
            geom_point(data = dfm.sub, size=2, colour = "brown") +
            labs(x = xlabel, y = ylabel, title = title)
    } else {
        ggplot(dfm, aes(index,marker, colour = as.factor(chrom_alt))) +
                geom_point(size=pointSize) +
                scale_x_continuous(breaks = xbreaks, labels = names(xbreaks), expand = c(0,0)) +
                geom_hline(aes(yintercept= as.numeric(sigLine)),colour = "red", size = 0.25) +
                scale_y_continuous(expand = c(0,0), limits = c(ymin,ymax))+
            guides(colour = FALSE) + labs(x = xlabel, y = ylabel, title = title)
    }
}
