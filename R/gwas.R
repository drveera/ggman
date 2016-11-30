#' A simulated GWAS dataset
#'
#' GWAS data simulated using plink 1.9 --simulate option.
#' 
#' @docType data
#' 
#' @usage data(gwas)
#' 
#' @format A dataframe with simulated GWAS results (simulated using plink 1.9)
#' 
#' @keywords datasets
#'
#' @examples
#' data(gwas)
#' p1 <- ggman(gwas, snp = "SNP", chrom = "CHR", bp = "BP", pvalue = "P")
#'
#' gwas.sig <- gwas[-log10(gwas$P)>8,]
#'
#' ggmanLabel(p1, labelDfm = gwas.sig, snp = "SNP", label = "SNP")
#' 
"gwas"
