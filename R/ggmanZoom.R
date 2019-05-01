#' ggmanZoom
#'
#' Zoom in to a specific region of the Manhattan Plot
#'
#' @param ggmanPlot A ggplot object of class 'ggman'
#' @param chromosome Chromosome identifier
#' @param start.position Starting basepair position
#' @param end.position Ending basepair position
#' @param gene.tracks if TRUE, refseq gene tracks will be downloaded frm UCSC server and plotted. default is TRUE
#' @param ymax maximum limit of y axis 
#' @param genome genome build version; default is 'hg19' 
#' @param exon.width width of the exons in the gene tracts; default is 0.5 
#' @param gene.width with of the introns in the gene tracts; default is 0.05
#' @param stack.level levels of stacking of the gene tracts; default is 1, where all the genes are plotted in single track
#' @param gene.text.size size of gene labes below the gene tracts; default is 2
#' @param remove.gene.text.overlap if TRUE, one of the overlapping gene text labels will be removed
#' @param track_guides default FALSE
#' @param xlabel X axis label 
#' @param ylabel Y axis label
#' @param title Plot title
#' @param point.legend.title point legend title
#' @param point.legend.name point legend name
#' @param ... other arguments to pass to \code{\link{geom_point}}
#' @return A regional association plot 
#' 
#' @examples
#'
#' #specific chromosome
#' p1 <- ggman(toy.gwas, snp = "snp", bp = "bp", chrom = "chrom",
#' pvalue = "pvalue")
#' ggmanZoom(p1, chromosome = 1)
#'
#' #specific region
#' ggmanZoom(p1, chromosome = 1, start.position = 215388741, end.position = 238580695)
#'
#' 
#' 
#' 
#'
#' @export
ggmanZoom <- function(
                      ggmanPlot,
                      chromosome,
                      start.position=NA,
                      end.position=NA,
                      gene.tracks = TRUE,
                      ymax=NA,
                      genome = "hg19",
                      exon.width = 0.5,
                      gene.width = 0.05,
                      stack.level=1,
                      remove.gene.text.overlap = FALSE,
                      track_guides = FALSE,
                      gene.text.size=2,
                      gene.position="bottom",
                      xlabel = NA,
                      ylabel = NA,
                      title = NA,
                      point.legend.title=NA,
                      point.legend.name=NA,
                      point.color="black",
                      stackfactor=1,
                      ...
                      ){
    ##check inputs
    environment(check.input.ggmanZoom) <- environment()
    check.input.ggmanZoom()
    
    dfm <- ggmanPlot[[1]]
    if (is.na(start.position)){
        dfm.sub <- dfm[dfm$chrom == chromosome,]
    } else {
        dfm.sub <- dfm[dfm$bp >= start.position & dfm$bp <= end.position & dfm$chrom == chromosome,]
    }
    
    if(is.na(title)){
        title <- "Regional association plot"
    }
    if(is.na(xlabel)){
        if(is.na(start.position)){
            xlabel = paste0("Chromosome",chromosome)
        } else {
            xlabel = paste0("Chromosome",chromosome,":",start.position,"-",end.position)
        }
    }

    if(is.na(ylabel)){
        ylabel = expression(paste("-log" ["10"],"P Value"))
    }
  ##ymax
  if(is.na(ymax)){
    ymax <- max(dfm.sub$marker+1)
  }
    p1 <- ggplot(dfm.sub, aes(bp,marker)) + geom_point(...) +
      labs(x = xlabel, y = ylabel, title = title)
  scm = c(point.color)
  names(scm) = point.legend.name
    if(gene.tracks){
        ##refseq
        environment(genetracks.refseq) <- environment()
        genetracks.refseq()
    } else {
      p1 <- list(plot=p1,
                 scm.title = point.legend.title,
                 scm = scm)
        return(p1)
    }
}
