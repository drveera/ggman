#' ggmanZoom_addpoints
#'
#' Zoom in to a specific region of the Manhattan Plot
#'
#' @param ggmanPlot A ggplot object of class 'ggman'
#' @param chromosome Chromosome identifier
#' @param start.position Starting basepair position
#' @param end.position Ending basepair position
#' @param ... other arguments to pass to \code{\link{geom_point}}
#' @return A regional association plot 
#'
#' @examples
#'
#'
#'
#'
#'
#'
#' @export
ggmanZoom_addpoints <- function(ggmanZoomPlot,
                                dfm,
                                snp="SNP",
                                pvalue="P",
                                point.color = "red",
                                point.legend.name="new analysis",
                                ...){
  ggmanZoomPlot1 <- ggmanZoomPlot[[1]]
  dfm0 <- ggmanZoomPlot1$data
  dfm <- as.data.frame(dfm)
  dfm1 <- data.frame(snp = dfm[,snp],pvalue=dfm[,pvalue])
  dfm <- merge(dfm0[,c("snp","index","bp")],dfm1,by="snp")
  dfm$marker <- -log10(dfm$pvalue)
  scmnames <- c(names(ggmanZoomPlot$scm),point.legend.name)
  scm <- c(ggmanZoomPlot$scm,point.color)
  names(scm) <- scmnames
  point.legend.title <- ggmanZoomPlot$point.legend.title
  p1 <- ggmanZoomPlot1 +
    geom_point(data=dfm,aes(color=point.legend.name), shape=5)+
    scale_color_manual(name=point.legend.title,
                       values=scm)
  class(p1) <- append(class(p1), "ggman")
  p1 <- list(plot=p1,
             scm=scm,
             point.legend.title=point.legend.title)
  return(p1)


}
