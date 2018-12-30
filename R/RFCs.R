#' Relative Frequency of Citation (RFC)
#'
#' This function allows you to calculate the relative frequency of citation (RFC) per species published by Pardo-de-Santayana (2003).
#' @source Pardo-de-Santayana, M. 2003. Las plantas en la cultura tradicional de la antigua Merindad de Campoo. Ph.D. dissertation, Departamento de Biología, Facul- tad de Ciencias, Universidad Autónoma de Madrid, Spain.
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' @keywords quantitative ethnobotany, number of uses
#'
#' @importFrom plyr ddply
#' 
#' @examples
#' RFCs(ethnobotanydata)
#' 
#' @export RFCs
RFCs <- function(data) {
    if (!requireNamespace("plyr", quietly = TRUE)) {
        stop("Package \"plyr\" needed for this function to work. Please install it.",
            call. = FALSE)
    }
  data$FCps <- rowSums((data[, -c(1:2)]) >
        0)
    data$FCps[data$FCps > 0] <- 1
    RFCs<-plyr::ddply(data, ~sp_name, plyr::summarise,
        RFCs = sum(FCps/(length(unique(informant)))))
    
    #change sort order
    RFCs <- RFCs[order(-RFCs$RFCs),] 
    
    print("Relative Frequency of Citation (RFC) for each species in the data set")
    print(RFCs)
    }

