#' highlights
#'
#' A vector of markers to highlight
#'
#' @docType data
#'
#' @usage data(highlights)
#'
#' @format A vector of snp IDs
#' 
#' @keywords datasets
#'
#' @examples
#' data(highlights)
#'
#' p1 <- ggman(gwas, snp = "SNP", chrom = "CHR", bp = "BP", pvalue = "P")
#'
#' ggmanHighlight(p1, highlight = highlights, colour = "red")
#'
"highlights"
