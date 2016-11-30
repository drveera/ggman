#' ggClumps
#'
#' Process the clumped file from plink
#'
#' @param plink.clumped A data frame read from plink.clumped file.
#'
#' @examples
#'
#' @export
ggClumps <- function(plink.clumped){
    x <- plink.clumped
    clump.snps <- x$SP2
    clump.snps <- as.character(clump.snps)
    clump.snps <- strsplit(clump.snps, split = ",")
    clump.snps <- lapply(clump.snps, function(x) gsub("\\(.*$","",x))
    names(clump.snps) <- x$SNP
    class(clump.snps) <- append(class(clump.snps), "ggclumps")
    return(clump.snps)
}
