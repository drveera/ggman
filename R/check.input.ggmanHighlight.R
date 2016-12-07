#' This function checks the input arguments to the function ggmanHighlight
#'
#' @keywords internal
#' 
check.input.ggmanHighlight <- function(){
    ## ggmanPlot input
    if(! any(class(ggmanPlot) == "ggman")){
        stop("The ggmanPlot input is not a ggman object")
    }
    ## highlight input
    if(! is.vector(highlight)){
        stop(paste0("The highlight input ",highlight," is not a vector object"))
    }

    ## colour input
        ## lineColor input
    ## Thanks to Sacha Epskamp for isColor function. 
    ## Reference:http://stackoverflow.com/questions/13289009/check-if-character-string-is-a-valid-color-representation/13290832
    isColor <- function(x)
    {
        res <- try(col2rgb(x),silent=TRUE)
        return(!"try-error"%in%class(res))
    }
    if(! isColor(colour)){
        stop(paste0("\'",colour,"\'"," is not a valid color"))
    }
    
}
