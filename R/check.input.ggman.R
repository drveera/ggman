#' This function checks the user input for the function ggman.R
#'
#' @keywords internal
#'
#' 
####checks###

check.input.ggman <- function(){
    ## gwas input
    if(!any(class(gwas) == "data.frame")){
        stop("The gwas input is not a data frame")
    }
    ## snp input
    if(class(snp) != "character"){
        stop("The snp input is not a character")
    }
    if(!snp %in% names(gwas)){
        stop(paste0("The column \'",snp,"\' is not present in the input data frame"))
    }
    ## bp input
    if(class(bp) != "character"){
        stop("The bp input is not a character")
    }
    if(! bp %in% names(gwas)){
        stop(paste0("The column \'",bp,"\' is not present in the input data frame"))
    }

    ##chrom input
    if(class(chrom) != "character"){
        stop("The chrom input is not a character")
    }
    if(! chrom %in% names(gwas)){
        stop(paste0("The column \'",chrom,"\' is not present in the input data frame"))
    }

    ## pvalue input
    if(class(pvalue) != "character"){
        stop("The pvalue input is not a character")
    }
    if(! pvalue %in% names(gwas)){
        stop(paste0("The column \'",pvalue,"\' is not present in the input data frame"))
    }

    ## sigline input
    if(!is.na(sigLine)){
        if(!is.numeric(sigLine)){
            stop("The sigLine input is not numeric")
        }
        if(sign(sigLine) == -1){
            stop("The sigLine argument cannot be negative")
        }
    }

    ## lineColour input
    ## Thanks to Sacha Epskamp for isColor function. 
    ## Reference:http://stackoverflow.com/questions/13289009/check-if-character-string-is-a-valid-color-representation/13290832
    isColor <- function(x)
    {
        res <- try(col2rgb(x),silent=TRUE)
        return(!"try-error"%in%class(res))
    }
    if(! isColor(lineColour)){
        stop(paste0("\'",lineColour,"\'"," is not a valid color"))
    }

    ## pointSize input
    if(! is.numeric(pointSize)){
        stop("The pointSize input is not numeric")
    }

    ## ymin input
    if(! is.numeric(ymin)){
        stop("The ymin input is not numeric")
    }

    ## ymax input
    if(!is.na(ymax)){
        if(! is.numeric(ymax)){
            stop("The ymax input is not numeric")
        }
    }

    ## logTransform input
    if(! is.logical(logTransform)){
        stop("The logTransform input is not logical")
    }

    ## relative.positions input
    if(! is.logical(relative.positions)){
        stop("The relative.positions input is not logical")
    }

    ## xlabel input
    if(!is.character(xlabel)){
        stop("The xlabel input is not character")
    }

    ## ylabel input
    if(!is.character(ylabel)){
        stop("The ylabel input is not character")
    }

    ## title
    if(! is.character(title)){
        stop("The title input is not character")
    }

    ##legend.title
    if(! is.character(legend.title)){
        stop("The legend title is not character")
    }

    ##clumps.label.type
    if(! is.character(clumps.label.type)){
        stop("The clumps.label.type is not character")
    }

    if(!any(clumps.label.type %in% c("label","text"))){
        stop("Invalid clumps.label.type input; specify either label or text ")
    }

    ##legend.remove
    if(! is.logical(legend.remove)){
        stop("The legend remove input is not logical")
    }
    
}
