utils::globalVariables(c("chromosome","start.position","end.position","highlight.group"))
#' This function checks the user input for the function ggmanZoom
#'
#' @keywords internal
#'
#' @return Nothing; internal function.
#'
#' 
#' 
####checks###

check.input.ggmanZoom <- function(){
    ## ggmanPlot input
    if(! any(class(ggmanPlot) == "ggman")){
        stop("The ggmanPlot input is not a ggman object")
    }

    ## chromosome input
    if(length(chromosome)>1){
        stop("Only one chromosome should be specified")
    }

    if(!chromosome %in% ggmanPlot[[1]]$chrom){
        stop(paste0("chromosome ",chromosome," is not present in the Manhattan ggplot layer"))
    }

    ## start position
    if(! is.na(start.position)){
           if(! is.numeric(start.position)){
        stop("The start.position is not numeric")
            }
    }


    ## end.position
    if(! is.na(end.position)){
        if(! is.numeric(end.position)){
            stop("The end.position is not numeric")
        }
    }

    ## xlabel
    if(! is.na(xlabel)){
        if(! is.character(xlabel)){
            stop("xlabel input is not character")
        }
    }

    ## ylabel
    if(! is.na(ylabel)){
        if(! is.character(ylabel)){
            stop("ylabel input is not character")
        }
    }

    ## title
    if(! is.na(title)){
        if(! is.character(title)){
            stop("title input is not character")
        }
    }

    
}
