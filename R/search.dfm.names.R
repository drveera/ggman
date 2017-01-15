
#' search.names
#'
#' @keywords internal
#'
#' @return Nothing; internal function
#' 
search.names <- function(term,dfmnames){
    for(i in 1:length(term)){
        res <- grep(paste0("\\b",term[i],"\\b"),dfmnames, ignore.case = TRUE)
        if(length(res)>0){
            if(length(res)==1){
                return(dfmnames[res])
            }
        }
    }
}
    


