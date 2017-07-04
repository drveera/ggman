#' plot.clumps
#'
#' @keywords internal
#'
#'
#' @return Nothing; internal function.
#'
#' 

plot.clumps <- function(){
    ##map the index snps with their clumps and make a data frame
    clump.dfm.lst <- list()
    for(i in 1:length(clumps$clump.index)){
        snp <- c(clumps$clump.index[i],unlist(clumps$clump.snps[i]))
        index.snp <- rep(clumps$clump.index[i], length(snp))
        if(! is.na(clumps$clump.groups)[1]){
            clump.groups <- rep(clumps$clump.groups[i],length(snp))
        } else {
            clump.groups <- rep(NA,length(snp))
        }
        if(! is.na(clumps$clump.labels)[1]){
            clump.labels <- rep(clumps$clump.labels[i],length(snp))
        } else {
            clump.labels <- rep(NA,length(snp))
        }
        clump.dfm.lst[[i]] <- cbind(snp,index.snp,clump.groups,clump.labels)
    }
    clump.dfm <- do.call(rbind,clump.dfm.lst)
    clump.dfm <- as.data.frame(clump.dfm)

    ##change the positions of clumped snps

    ##create a seperate dfm for index snp
    indexSnps <- clumps$clump.index
    index.dfm <- dfm[dfm$snp %in% indexSnps,]

    ##get the grouping and label info from clump.dfm
    group.dfm <- clump.dfm[,c("snp","clump.groups","clump.labels")]
    index.dfm <- merge(index.dfm,group.dfm,by="snp")
    index.dfm <- index.dfm[!duplicated(index.dfm$snp),]
    ##change the snp column name to index.snp
    index.dfm$index.snp <- index.dfm$snp

    ## get the pvalues for clump dfm
    pvalues.dfm <- dfm[,c("snp","pvalue","marker")]
    clump.dfm <- merge(clump.dfm,pvalues.dfm, by = "snp")
    ## get the positions
    positions.dfm <- dfm[,c("snp","index","chrom")]
  names(positions.dfm)[1] <- "index.snp"
  dfm.sub <- merge(clump.dfm,positions.dfm,by = "index.snp",all.x=TRUE)
    ##use dfm.sub to plot the clumps and dfm to plot all other points
    ##remove the clumped snps from main dfm
    dfm <- dfm[!dfm$snp %in% clump.dfm$snp,]




    ##plot
    ##plot1
    p1 <- ggplot(dfm, aes(index,marker, colour = as.factor(chrom_alt))) +
        geom_point(size=pointSize) +
        scale_x_continuous(breaks = xbreaks, labels = names(xbreaks), expand = c(0,0)) +
        geom_hline(aes(yintercept= as.numeric(sigLine)),colour = "red", size = 0.25) +
        scale_y_continuous(expand = c(0,0), limits = c(ymin,ymax))  +
        guides(colour = FALSE) +
        scale_colour_grey(start = 0.4,end=0.6) 

    ##plot2
    if(is.na(clumps$clump.groups)[1]){

        p1 <-  p1 + geom_point(data = dfm.sub, size=pointSize, colour = clumps.colour) +
            geom_point(data = index.dfm, size = pointSize+1, colour = clumps.colour, shape = 5) +
            labs(x = xlabel, y = ylabel, title = title)
    } else {

        p1 <-  p1 + geom_point(data = dfm.sub, aes(fill = as.factor(clump.groups)),size=pointSize, shape = 21,
                               colour = alpha("black",0)) +
            geom_point(data = index.dfm, aes(fill = as.factor(clump.groups)),size = pointSize+1, shape =23,
                               colour = alpha("black",0)) +
            labs(x = xlabel, y = ylabel, title = title, fill = legend.title)
    }


    if(!is.na(clumps$clump.labels)[1]){
        if(clumps.label.type == 'label'){
            p1 <- p1 + geom_label_repel(data = index.dfm, aes(x = index, y = marker,label = clump.labels),colour = "black")
        } else {
            p1 <- p1 + geom_text_repel(data = index.dfm, aes(x = index, y = marker,label = clump.labels),colour = "black")
        }

    }

    if(legend.remove){
        p1 <- p1 + guides(fill = FALSE)
    }
return(list(p1,dfm.sub))
}
