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
                        index.snp = "index.snp",
                        clumps = "clumps"
                        ){
    x <- plink.clumped
    clump.snps <- x[,clumps]
    clump.snps <- as.character(clump.snps)
    clump.snps <- strsplit(clump.snps, split = ",")
    clump.snps <- lapply(clump.snps, function(x) gsub("\\(.*$","",x))
    names(clump.snps) <- x[,index.snp]
    class(clump.snps) <- append(class(clump.snps), "ggclumps")
    return(clump.snps)
}
