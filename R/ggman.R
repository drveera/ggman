#' Creates a Manhattan Plot.
#'
#' Creates a Manhattan Plot as a ggplot layer.
#'
#' The function creates a manhattan plot as a ggplot layer. The output can be stored in a variable,
#' and additional layers can be added. See \code{\link{ggmanHighlight}}, \code{\link{ggmanZoom}} and
#'  \code{\link{ggmanLabel}}.
#' 
#' @param gwas A data frame with the gwas results
#' @param clumps Optional parameter; An object of class 'ggclumps' containing
#' the SNP clumps, see \code{\link{ggclumps}}
#' @param snp Name of the column containing SNP identifiers; default is 'snp'
#' @param bp Name of the column containing the basepair positions; default is 'bp'
#' @param chrom Name of the column containing the chromosome identifers; default is 'chrom'
#' @param pvalue Name of the column containing the p values; default is 'pvalue'
#' @param sigLine Threshold for the genome wide significant line in -log10 scale; default is 8
#' @param lineColor colour of the genomewide line; default is red
#' @param pointSize Size of the points in the plot supplied to geom_point
#' @param ymin Starting point of y axis; default is 0
#' @param ymax Ending point of y axis
#' @param logTransform if TRUE, P value is log10 transformed; default is TRUE; Specify FALSE
#' when plotting values other p values, such as zscore or beta
#' @param relative.positions if TRUE, the gaps between the SNPs will be reflected in the X-axis points
#' @param xlabel X-axis label
#' @param ylabel Y-axis label
#' @param title plot title
#' @param ... other arguments to pass to \code{\link[ggplot2]{geom_point}};
#' Note: do not use \emph{size} to specify size of the points, instead use \emph{pointSize}
#'
#' @import ggplot2
#' 
#' @importFrom gtools mixedorder
#' 
#' 
#' @examples
#'
#' #basic plot
#' ggman(gwas,snp = "SNP", bp ="BP", chrom = "CHR", pvalue = "P")
#'
#' 
#' \dontrun {
#' #with relative positions
#' ggman(gwas,snp = "SNP", bp ="BP", chrom = "CHR", pvalue = "P", relative.positions = TRUE)
#' #with clumps
#' ggman(gwas,snp = "SNP", bp ="BP", chrom = "CHR", pvalue = "P", clumps = gwasclumps)
#' }
#'
#' @export
ggman <- function(gwas,
                  clumps = NA,
                  snp = "snp", bp = "bp", chrom = "chrom", pvalue = "pvalue",
                  sigLine = 8,
                  lineColor = "red",
                  pointSize = 0.1,
                  ymin = 0, ymax = NA,
                  logTransform = TRUE,
                  relative.positions = FALSE,
                  xlabel = "chromosome", ylabel = "-log10(P value)", title = "Manhattan Plot") {

    ###check the inputs
    environment(check.input.ggman) <- environment()
    check.input.ggman()

    gwas <- as.data.frame(gwas)
    chrom <- gwas[,chrom]
    bp <- as.numeric(as.character(gwas[,bp]))
    pvalue <- as.numeric(as.character(gwas[,pvalue]))
    snp <- gwas[,snp]
    dfm <- data.frame(chrom,bp,snp,pvalue)
    dfm$chrom <- as.character(dfm$chrom)

    ##add index
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

    if(logTransform){
        dfm$marker <- -log10(dfm$pvalue)
    } else {
        dfm$marker <- dfm$pvalue
    }

    if (is.na(ymax)){
        ymax <- max(-log10(dfm$pvalue)) + 1
    }

    if(relative.positions){
        relpos <- function(x,minbp,maxbp,nrows,startingpoint){
            actual.distance <- (x - minbp)
            relative.distance <- (actual.distance*nrows)/maxbp
            return(relative.distance + startingpoint)
        }
        dfm$chrom <- factor(dfm$chrom, levels = chrtable$Var1)
        dfm.split <- split(dfm, dfm$chrom)
        startingpoint = 1
        dfm.list <- lapply(dfm.split, function(x){         
            minbp <- as.numeric(min(x$bp))
            maxbp <- as.numeric(max(x$bp))
            nrows <- as.numeric(nrow(x))
            x$index <- relpos(x$bp,minbp,maxbp,nrows,startingpoint)
            startingpoint <<- max(x$index)+1          
            return(x)
        })
        dfm <- do.call(rbind,dfm.list)
        
    }

    ##create x axis tick points
    dfmsplit <- split(dfm, dfm$chrom)
    xbreaks <- sapply(dfmsplit,function(x) x$index[length(x$index)/2])


    
    if (!is.na(clumps)[1]){
        if (!any(class(clumps) == "ggclumps")) {
            stop("clumps argument takes an object of class ggclumps;see ggClumps function")
        }
        clumpedSnps <- unlist(clumps)
        indexSnps <- names(clumps)

        ##index snps subset
        index.dfm <- dfm[dfm$snp %in% indexSnps,]
        
        clumpedSnps <- c(clumpedSnps, indexSnps )
        index <- dfm$index
        names(index) <- dfm$snp

        pb <- txtProgressBar(min = 0, max = length(indexSnps), style = 3)
        j = 0
        for (i in indexSnps){
            index <- replace(index, names(index) %in% clumps[[which(names(clumps) == i)]], index[i])
            j = j + 1
            setTxtProgressBar(pb, j)
        }
        close(pb)
        dfm$index <- index
        dfm.sub <- dfm[dfm$snp %in% clumpedSnps,]
        dfm.index <- dfm[dfm$snp %in% indexSnps,]
        
        ggplot(dfm, aes(index,marker, colour = as.factor(chrom_alt))) +
            geom_point(size=pointSize) +
            scale_x_continuous(breaks = xbreaks, labels = names(xbreaks), expand = c(0,0)) +
            geom_hline(aes(yintercept= as.numeric(sigLine)),colour = "red", size = 0.25) +
            scale_y_continuous(expand = c(0,0), limits = c(ymin,ymax))  +
            guides(colour = FALSE) +
            scale_colour_grey(start = 0.4,end=0.6) +
            geom_point(data = dfm.sub, size=pointSize, colour = "blue") +
            geom_point(data = index.dfm, size = pointSize+1, colour = "blue", shape = 5) +
            labs(x = xlabel, y = ylabel, title = title)
    } else {
        ggplot(dfm, aes(x = index,y = marker, colour = as.factor(chrom_alt))) +
                geom_point(size=pointSize) +
            scale_x_continuous(breaks = xbreaks, labels = names(xbreaks),
                               expand = c(0,0)) +
            geom_hline(aes(yintercept= as.numeric(sigLine)),
                       colour = "red", size = 0.25) +
            scale_y_continuous(expand = c(0,0), limits = c(ymin,ymax))+
            guides(colour = FALSE) +
             labs(x = xlabel, y = ylabel, title = title) 
    }
}
