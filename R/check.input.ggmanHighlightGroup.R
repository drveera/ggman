
utils::globalVariables(c("length.remove","highlightDfm"))
#' This function checks the input for the function ggmanHighlightGroup
#'
#' @keywords internal
#'
#' 
check.input.ggmanHighlightGroup <- function(){
        ## ggmanPlot input
    if(! any(class(ggmanPlot) == "ggman")){
        stop("The ggmanPlot input is not a ggman layer")
    }

        ## labelDfm input
    if(! any(class(highlightDfm) == "data.frame")){
        stop("The highlightDfm input is not a data frame")
    }

    ## snp input
    if(! is.character(snp)){
        stop("The snp input is not a character")
    }
    
    if(!snp %in% names(highlightDfm)){
        stop(paste0("The column \'",snp,"\' is not present in the input highlight data frame"))
    }
    
    ## group input
    if(! is.character(group)){
        stop("The group input is not a character")
    }

    if(! group %in% names(highlightDfm)){
        stop(paste0("The column \'",snp,"\' is not present in the input data frame"))
    }

    ## legend.title
    if(! is.character(legend.title)){
        stop("The legend.title input is not character")
    }

    ## legend.remove
    if(! is.logical(legend.remove)){
        stop("The legend.remove input is not logical")
    }
    
}
