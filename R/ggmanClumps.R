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
