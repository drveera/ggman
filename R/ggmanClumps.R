#' ggmanClumps
#'
#' Process the clumped file from plink
#'
#' @param plink.clumped A data frame read from plink.clumped file.
#'
#' @examples
#'
#' @export
ggmanClumps <- function(plink.clumped,
                        index.snp.column,
                        clumps.column,
                        color.column = NA,
                        label.column = NA,
                        group.column = NA
                        ){

    clump.snps <- plink.clumped[,clumps.column]
    clump.snps <- as.character(clump.snps)
    clump.snps <- strsplit(clump.snps, split = ",")
    clump.snps <- lapply(clump.snps, function(x) gsub("\\(.*$","",x))
    ##names(clump.snps) <- x[,index.snp.column]

    ##index snp
    clump.index <- as.character(plink.clumped[,index.snp.column])

    ## color
    if(! is.na(color.column)){
        clump.colors <- as.character(plink.clumped[,color.column])
    } else {
        clump.colors <- NA
    }

    ##labels
    if(! is.na(label.column)){
        clump.labels <- as.character(plink.clumped[,label.column])
    } else {
        clump.labels <- NA
    }

    ##group.column
    if(!is.na(group.column)){
        clump.groups <- as.character(plink.clumped[,group.column])
    } else {
        clump.groups <- NA
    }
    clumps <- list(clump.snps,clump.index,clump.colors,clump.labels,clump.groups)
    names(clumps) <- c("clump.snps","clump.index","clump.colors","clump.labels","clump.groups")
    class(clumps) <- append(class(clump.snps), "ggmanClumps")
    return(clumps)
}
