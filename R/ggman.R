
# global variables to escape r cmd check
utils::globalVariables(c("index","marker","chrom_alt","group","xbreaks"))

#' Creates a Manhattan Plot.
#'
#' Creates a Manhattan Plot as a ggplot layer.
#'
#' The function creates a manhattan plot as a ggplot layer. The output can be stored in a variable,
#' and additional layers can be added. See \code{\link{ggmanHighlight}}, \code{\link{ggmanZoom}} and
#'  \code{\link{ggmanLabel}}.
#' 
#' @param gwas A data frame with the gwas results
#' @param clumps Optional argument; takes an object of class 'ggclumps' containing
#' the SNP clumps, see \code{\link{ggmanClumps}}
#' @param clumps.colour colour of the clumps
#' @param snp Name of the column containing SNP identifiers; default is 'snp'
#' @param bp Name of the column containing the basepair positions; default is 'bp'
#' @param chrom Name of the column containing the chromosome identifers; default is 'chrom'
#' @param pvalue Name of the column containing the p values; default is 'pvalue'
#' @param sigLine Threshold for the genome wide significant line in -log10 scale; default is 8; specify NA to remove the line.
#' @param lineColour colour of the genomewide line; default is 'red'
#' @param pointSize Size of the points in the plot; default is 0.1
#' @param ymin Starting point of y axis; default is 0
#' @param ymax Ending point of y axis
#' @param logTransform if TRUE, P value is -log10 transformed; default is TRUE; Specify FALSE
#' when plotting values other p values, such as zscore or beta
#' @param invert if TRUE, an inverted Manhattan Plot will be created. The p values of the variants with (or < 1 or beta < 0) will positive log10-transformed, which will result in negative values. 
#' @param invert.method whether inversion should be based on odds ratio or beta. possible values: 'or' or 'beta'
#' @param invert.var name of the column in the gwas data.frame containing the or or beta
#' @param relative.positions if TRUE, the x axis points will be calculated in proportion to the basepair positions. So, the gaps in the genome with no genotypes will be reflected in the plot(requires more computation, hence more time to plot). If FALSE, the SNPs are ascendingly sorted chromosome-wise. The default value is FALSE. 
#' @param xlabel X-axis label
#' @param ylabel Y-axis label
#' @param title plot title
#' @param legend.title title of legend; this argument applies only when the clumps are plotted and highlighted in groups.
#' @param clumps.label.type type of label; either 'text' or 'label'; default is 'label'
#' @param legend.remove if TRUE, the  legend will be removed; default is FALSE
#' @param ... other arguments to pass to \code{\link[ggplot2]{geom_point}};
#' Note: do not use \emph{size} to specify size of the points, instead use \emph{pointSize}
#'
#' @import ggplot2
#' @import ggrepel
#' 
#' @importFrom gtools mixedorder
#' 
#'
#' @return A Manhattan Plot
#'
#' 
#' @examples
#'
#'#simple Manhattan Plot
#' ggman(toy.gwas, snp = "snp", bp = "bp", chrom = "chrom",
#' pvalue = "pvalue")
#'
#' #enable relative positioning
#' ggman(toy.gwas, snp = "snp", bp = "bp", chrom = "chrom",
#' pvalue = "pvalue",relative.positions = TRUE)
#'
#' #plot odds ratio
#' ggman(toy.gwas, snp = "snp", bp = "bp", chrom = "chrom",
#' pvalue = "or",logTransform = FALSE, ymax = 3)
#' 
#' #plot beta
#' ggman(toy.gwas, snp = "snp", bp = "bp", chrom = "chrom", pvalue = "beta",
#' logTransform = FALSE, ymin = -2, ymax = 2)
#'
#' #inverted Manhattan plot 
#' ggman(toy.gwas, snp = "snp", bp = "bp", chrom = "chrom", pvalue = "pvalue",
#' invert = TRUE, invert.method = 'or', invert.var = "or")
#' 
#'
#' 
#' 
#'
#' 
#'
#'
#' @export
ggman <- function(gwas,
                  clumps = NA,
                  clumps.colour = "blue",
                  snp = NA, bp = NA, chrom = NA, pvalue = NA,
                  sigLine = 8,
                  lineColour = "red",
                  pointSize = 0.1,
                  ymin = NA, ymax = NA,
                  logTransform = TRUE,
                  invert = FALSE, invert.method='or',invert.var='or',
                  relative.positions = FALSE,
                  xlabel = "chromosome", ylabel = "-log10(P value)", title = "Manhattan Plot",
                  legend.title = "legend", clumps.label.type = 'label', legend.remove = FALSE, ...) {

    ##define global variables to escape R CMD check
   

    dfmnames <- names(gwas)
        ## chrom input
    if(is.na(chrom)){
        chrom <- search.names(c("chr","chrom","chromosome"), dfmnames)
        if(is.null(chrom)){
            stop("Couldn't find the chromosome column.
Specify the name of the column with chromosome ids")
        }
    }

        if(is.na(snp)){
        snp <- search.names(c("snp","rsid"), dfmnames)
        if(is.null(snp)){
            stop("Couldn't find the snp column.
Specify the name of the column with snp ids")
        }
        }

        if(is.na(bp)){
        bp <- search.names(c("bp","basepair","position","start"),dfmnames)
        if(is.null(bp)){
            stop("Couldn't find the bp column.
Specify the name of the column with bp ids")
        }
        }
        if(is.na(pvalue)){
        pvalue <- search.names(c("p","pval","pvalue"), dfmnames)
        if(is.null(pvalue)){
            stop("Couldn't find the pvalue column.
Specify the name of the column with pvalues")
        }
        }

    
    ##check the inputs
    environment(check.input.ggman) <- environment()
    check.input.ggman()

    dfm <- as.data.frame(gwas)
    dfm$chrom <- dfm[,chrom]
    dfm$bp <- as.numeric(as.character(dfm[,bp]))
    dfm$pvalue <- as.numeric(as.character(dfm[,pvalue]))
    dfm$snp <- dfm[,snp]
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

    ##invert
    if(invert){
        if(invert.method == 'or'){                        
            dfm$or <- as.numeric(as.character(dfm[,invert.var]))
            dfm$sign <- with(dfm, replace(pvalue,or > 1, 1))
            dfm$sign <- with(dfm, replace(sign, sign != 1, -1))
        } else {
            dfm$beta <- as.numeric(as.character(dfm[,invert.var]))
            dfm$sign <- with(dfm, replace(beta, beta > 0, 1))
            dfm$sign <- with(dfm, replace(sign, sign != 1, -1))
        }
        dfm$marker <- with(dfm, -log10(pvalue) * sign)
        ##axis
        if(is.na(ymin)){
            ymin = ymax * -1
        }        
    }
    

    if (!is.na(clumps)[1]){
        if (!any(class(clumps) == "ggmanClumps")) {
            stop("clumps argument takes an object of class ggmanclumps;see ggmanClumps function")
        }
        environment(plot.clumps) <- environment()
        plot.clumps()        
    } else {
        p1 <- ggplot(dfm, aes(x = index,y = marker, colour = as.factor(chrom_alt))) +
                geom_point(size=pointSize) +
            scale_x_continuous(breaks = xbreaks, labels = names(xbreaks),
                               expand = c(0,0)) +
            scale_y_continuous(expand = c(0,0), limits = c(ymin,ymax))+
            guides(colour = FALSE) +
            labs(x = xlabel, y = ylabel, title = title)

    if(!is.na(sigLine)){
        p1 <- p1 + geom_hline(aes(yintercept= as.numeric(sigLine)),
                        colour = lineColour, size = 0.25) 
    }
        if(invert){
            p1 <- p1 + geom_hline(aes(yintercept= as.numeric(sigLine) * -1),
                                  colour = lineColour, size = 0.25) +
                geom_hline(aes(yintercept= 0),colour = "white", size = 0.25)
        }
        class(p1) <- append(class(p1), "ggman")
        return(p1)
    }
    
}
