#' refseq
#'
#' @keywords internal
#'
#'@return internal function.
#' 
####checks###

genetracks.refseq <- function(){
    library(rtracklayer)
    ##chromosome = 'X'
    ##start.position = 33031597
    ##end.position = 35051570

    ##get the chromosome number
    ##chrom.number <- gsub("[^[:digit:]]", "", chromosome)
    ##create a range object
    myrange <- GRanges(paste0("chr",chromosome), IRanges(start.position,end.position))

    ##query
    mysession = browserSession("UCSC")
    ##to check which tracks are available
    ##track.names <- trackNames(ucscTableQuery(mySession))
    ##track.names[grep("Gene",names(track.names))]
    ##get table  names
    ##tableNames(ucscTableQuery(mysession, track="ensGene"))
    genome(mysession) <- genome
    ##trackname <- "ensGene"
    trackname <- "wgEncodeGencodeV19"
    tablename <- "wgEncodeGencodeBasicV19"
    mytable <- getTable(ucscTableQuery(mysession,
                                       track = trackname,
                                       table=tablename,
                                       range=myrange))

    ##column classes
    mytable$exonStarts <- as.character(mytable$exonStarts)
    mytable$exonEnds <- as.character(mytable$exonEnds)

    ## calculate the length of the transcripts
    mytable$genesize <- with(mytable,txEnd-txStart)
    mytable$midpoint <- with(mytable, txStart + (genesize/2))

    ##make all positive
    mytable$genesize <- with(mytable, ifelse(genesize<0,-genesize,genesize))

    ## sort the table according to txStart position
    mytable <- mytable[order(mytable$txStart),]

    ## list the name2
    name2 <- with(mytable, as.character(name2[!duplicated(name2)]))

    stacks <- rep((-1 * (1:stack.level)),length(name2))[1:length(name2)]
    stack.dfm <- data.frame(name2,stacks)

    ##add to mytable
    mytable <- merge(mytable,stack.dfm,by = "name2", all.x = TRUE, sort = FALSE)
    
    ## split with name2
    mytable.split <- split(mytable, mytable$name2)

    ##get the longest transcript
    mytable.split <- lapply(mytable.split, function(x) {
        dfm <- x[x$genesize == max(x$genesize)[1],]
        dfm <- dfm[!duplicated(dfm$name2),]
    })


    genetable <- do.call(rbind,mytable.split)

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
    exontable <<- do.call(rbind,mytable.split)
    ##column classes
    exontable$exon.start <- as.numeric(as.character(exontable$exon.start))
    exontable$exon.end <- as.numeric(as.character(exontable$exon.end))


    ##calculate genetracks size
    genetable$gene.ymax <- genetable$stacks + (gene.width/2)
    genetable$gene.ymin <- genetable$stacks - (gene.width/2)
    exontable$exon.ymax <- exontable$stacks + (exon.width/2)
    exontable$exon.ymin <- exontable$stacks - (exon.width/2)


    ##plot
    p1 <- p1 + geom_hline(yintercept = 0, colour="grey", width=0.5)
    p1 <-
        p1+
        geom_rect(data= genetable,
                  aes(xmin = txStart, xmax=txEnd, ymin = gene.ymin,
                      ymax = gene.ymax, fill = as.factor(strand)),inherit.aes = FALSE) +
        geom_rect(data = exontable,
                  aes(xmin=exon.start,xmax=exon.end, ymin = exon.ymin,
                      ymax = exon.ymax, fill = as.factor(strand)),inherit.aes = FALSE) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "grey")) +
        scale_fill_grey(start=0.3,end=0.7) + labs(fill="strand")
    if (gene.position == "bottom"){
            p1 <-  p1 + geom_text(data = genetable,aes(x = midpoint,y=-0.3+gene.ymin, label = name2, angle = 0), size = gene.text.size, nudge_x = 0, nudge_y =0,
                   check_overlap = remove.gene.text.overlap, inherit.aes = FALSE) 
    } else if (gene.position == "left"){
            p1 <-  p1 + geom_text(data = genetable,aes(x = txStart - 10000 ,y = gene.ymin + (gene.ymax - gene.ymin)/2, label = name2, angle = 0, hjust = "right"), size = gene.text.size, nudge_x = 0, nudge_y =0,
                   check_overlap = remove.gene.text.overlap, inherit.aes = FALSE) 
    } else if (gene.position == "right"){
            p1 <-  p1 + geom_text(data = genetable,aes(x = txEnd + 10000, y=gene.ymin + (gene.ymax - gene.ymin)/2, label = name2, angle = 0, hjust = "left"), size = gene.text.size, nudge_x = 0, nudge_y =0,
                   check_overlap = remove.gene.text.overlap, inherit.aes = FALSE) 
    }
    if (track_guides){
            p1 <- p1 + geom_rect(data = genetable,aes(ymin = gene.ymin, ymax = ymax,
                                       xmin = txStart, xmax= txEnd,fill = as.factor(strand)),
                                 alpha = 0.02, inherit.aes = FALSE) +
            geom_rect(data = exontable, aes(xmin=exon.start,xmax=exon.end, ymin = exon.ymin,
                                                ymax = ymax, fill = as.factor(strand)),
                          alpha = 0.03,inherit.aes = FALSE) 
    }
    p1 + scale_y_continuous(breaks = 0:ymax+1, labels = 0:ymax+1, limits = c(-1-(as.numeric(stack.level)), ymax)) 
}
