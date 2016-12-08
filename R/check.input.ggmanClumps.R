
utils::globalVariables(c("plink.clumped","index.snp.column","clumps.column","label.column","group.column","group.column"))
#' This function checks the user input for the function ggmanClumps.R
#'
#' @keywords internal
#'
#' 
####checks###

check.input.ggmanClumps <- function(){
    ##plink.clumped
    if(! any(class(plink.clumped) == 'data.frame')){
        stop("The plink.clumped input is not a data.frame")
    }
    ##index.snp.column
    if(! is.character(index.snp.column)){
        stop("The index.snp.column input is not character")
    }

    if(! index.snp.column %in% names(plink.clumped)){
        stop(paste0("The column ",index.snp.column," is not present in the plink.clumped data.frame"))
    }

    ##clumps.column
    if(! is.character(clumps.column)){
        stop("The clumps.column input is not character")
    }
    if(! clumps.column %in% names(plink.clumped)){
        stop(paste0("The column ",clumps.column," is not present in the plink.clumped data.frame"))
    }

    ##label.column
    if(! is.na(label.column)){        
        if(! is.character(label.column)){
            stop("The label.column input is not a character")
        }
        if(! label.column %in% names(plink.clumped)){
            stop(paste0("The column ",label.column," is not present in the plink.clumped data.frame"))
        }
    }
    
    
    ##group.column
    if(! is.na(group.column)){
        if(! is.character(group.column)){
            stop("The group.column input is not character")
            }
            if(! group.column %in% names(plink.clumped)) {
                stop(paste0("The column ",group.column," is not present in the plink.clumped data.frame"))
            }
        }
}
