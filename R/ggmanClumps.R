#' ggmanClumps
#'
#' Process the clumped file from plink.
#'
#' @param plink.clumped A data frame read from plink.clumped file.
#' @param index.snp.column Name of the column containing the index SNPs
#' @param clumps.column Name of the column containing the clumps
#' @param label.column Name of the column containing the labels
#' @param group.column Name of the column containing the grouping variable 
#'
#' @examples
#'
#' #just the clumps, with no label and no highlights
#' toy.clumps <- ggmanClumps(toy.clumped, index.snp.column = "SNP", clumps.column = "SP2")
#'
#' #add labels and highlights
#' toy.clumps <- ggmanClumps(toy.clumped, index.snp.column = "SNP", clumps.column = "SP2",
#'                           group.column = "group", label.column = "label")
#' 
#' @export
ggmanClumps <- function(plink.clumped,
                        index.snp.column,
                        clumps.column,
                        label.column = NA,
                        group.column = NA
                        ){

    ##check inputs
    environment(check.input.ggmanClumps) <- environment()
    check.input.ggmanClumps()

    clump.snps <- plink.clumped[,clumps.column]
    clump.snps <- as.character(clump.snps)
    clump.snps <- strsplit(clump.snps, split = ",")
    clump.snps <- lapply(clump.snps, function(x) gsub("\\(.*$","",x))
    ##names(clump.snps) <- x[,index.snp.column]

    ##index snp
    clump.index <- as.character(plink.clumped[,index.snp.column])


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
    clumps <- list(clump.snps,clump.index,clump.labels,clump.groups)
    names(clumps) <- c("clump.snps","clump.index","clump.labels","clump.groups")
    class(clumps) <- append(class(clump.snps), "ggmanClumps")
    return(clumps)
}
