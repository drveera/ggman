#' refseq
#'
#' @keywords internal
#'
#'@return internal function.
#' 
####checks###

genetracks.refseq <- function(){
    library(rtracklayer)
    #chromosome = 21
    #start.position = 33031597
    #end.position = 35051570

    ##get the chromosome number
    chrom.number <- gsub("[^[:digit:]]", "", chromosome)
    ##create a range object
    myrange <- GRanges(paste0("chr",chrom.number), IRanges(start.position,end.position))

    ##query
    mysession = browserSession("UCSC")
    genome(mysession) <- "hg19"

    mytable <- getTable(ucscTableQuery(mysession,
                                       track = "refGene",
                                       table="refGene",
                                       range=myrange))

    ##column classes
    mytable$exonStarts <- as.character(mytable$exonStarts)
    mytable$exonEnds <- as.character(mytable$exonEnds)

    ## calculate the length of the transcripts
    mytable$genesize <- with(mytable,txEnd-txStart)
    mytable$midpoint <- with(mytable, txStart + (genesize/2))

    ##make all positive
    mytable$genesize <- with(mytable, ifelse(genesize<0,-genesize,genesize))

    ## split with name2
    mytable.split <- split(mytable, mytable$name2)

    ##get the longest transcript
    mytable.split <- lapply(mytable.split, function(x) {
        return(x[x$genesize == max(x$genesize)[1],])
    })

    genetable <- do.call(rbind,mytable.split)
    genetable <- genetable[!duplicated(genetable$name2),]

    ##melt in to exons
    mytable.split <- lapply(mytable.split, function(x){
        exon.start <- unlist(strsplit(x$exonStarts, split = ","))
        exon.end <- unlist(strsplit(x$exonEnds, split = ","))
        name2 <- rep(x$name2,length(exon.start))
        dfm <- data.frame(name2,exon.start,exon.end)
        dfm.new <- merge(dfm,x,all.x = TRUE)
        return(dfm.new)
    })

    ##combine in to sigle data frame
    exontable <- do.call(rbind,mytable.split)
    ##column classes
    exontable$exon.start <- as.numeric(as.character(exontable$exon.start))
    exontable$exon.end <- as.numeric(as.character(exontable$exon.end))

    ##plot
#    return(
#        p1 +
#       geom_rect(data= genetable, aes(xmin = txStart, xmax=txEnd,ymin = -0.01, ymax = 0.01),inherit.aes = FALSE)
#    )


    p1+
        geom_rect(data= genetable, aes(xmin = txStart, xmax=txEnd,
                                       ymin = -1.01, ymax = -0.99),inherit.aes = FALSE) +
        geom_rect(data = exontable, aes(xmin=exon.start,xmax=exon.end,
                                        ymin = -1.05, ymax = -0.95),inherit.aes = FALSE) +
        geom_rect(data = genetable,aes(ymin = -1.01, ymax = ymax,
                                       xmin = txStart, xmax= txEnd),
                  alpha = 0.05, inherit.aes = FALSE) +
        geom_rect(data = exontable, aes(xmin=exon.start,xmax=exon.end, ymin = -1.05,
                                        ymax = ymax), alpha = 0.02,inherit.aes = FALSE) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "grey")) +
        geom_text(data = genetable,aes(x = midpoint,y=-2, label = name2, angle = 90), size = 2, nudge_x = 0, nudge_y =0, check_overlap = FALSE)
}
