#' Fidelity Level (FL)
#' 
#' Calculates the fidelity level (FL) of species uses,  i.e. the ratio between the number of informants who independently cite the use of a species for the same major purposes (URs) and the total number of informants who mentioned the plant for any use (FCs). 
#' @source Friedman, J., Z. Yaniv, A. Dafni, and D. Palewitch. 1986. “A Preliminary Classification of the Healing Potential of Medicinal Plants, Based on a Rational Analysis of an Ethnopharmacological Field Survey Among Bedouins in the Negev Desert, Israel.” Journal of Ethnopharmacology 16 (2-3): 275–87.
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' 
#' @importFrom dplyr filter summarize select left_join group_by slice
#' @importFrom magrittr %>%
#' @importFrom reshape melt
#' 
#' @keywords arith math logic methods misc survey
#'
#' @return Data frame of species and fidelity level (FL) values.
#'
#'@section Warning:
#' 
#' Identification for informants and species must be listed by the names 'informant' and 'sp_name' respectively in the data set.
#' The rest of the columns should all represent separate identified ethnobotany use categories. These data should be populated with counts of uses per informant (should be 0 or 1 values).
#' 
#' @examples
#' 
#' #Use built-in ethnobotany data example
#' FLs(ethnobotanydata)
#' #returns the primary use category (Primary.use) and the FLs value
#' 
#' #Generate random dataset of three informants uses for four species
#' 
#' eb_data <- data.frame(replicate(10,sample(0:1,20,rep=TRUE)))
#' names(eb_data) <- gsub(x = names(eb_data), pattern = "X", replacement = "Use_")  
#' eb_data$informant <- sample(c('User_1', 'User_2', 'User_3'), 20, replace=TRUE)
#' eb_data$sp_name <- sample(c('sp_1', 'sp_2', 'sp_3', 'sp_4'), 20, replace=TRUE)
#' 
#' FLs(eb_data)
#' 
#' @export FLs
#' 
FLs <- function(data) {
  
  #Add error stops ####
   #Check that packages are loaded
    {
  if (!requireNamespace("reshape", quietly = TRUE)) {
    stop("Package \"reshape\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package \"dplyr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("magrittr", quietly = TRUE)) {
    stop("Package \"magrittr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
    }# end package check
    
    ## Check that use categories are greater than zero
    if (!any(sum(dplyr::select(data, -informant, -sp_name)>0))){
      warning("The sum of all UR is not greater than zero. Perhaps not all uses have values or are not numeric.")
      data<-data[stats::complete.cases(data), ]
    }
    
    ## Use 'complete.cases' from stats to get to the collection of obs without NA
    if (any(is.na(data))) {
      warning("Some of your observations included \"NA\" and were removed. Consider using \"0\" instead.")
      data<-data[stats::complete.cases(data), ]
    } #end error stops
  
  # Set the variables to NULL first, appeasing R CMD check
  URspecies <- URcategory <- variable <-  value <- URspdata <- melt_FLS <- FLs <- URdata <- UR_sum <- sp_name <- informant <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
  
  FLsdata <- data #create subset-able data
  
  Iu <- FCs(FLsdata) #calculate Iu, same as FCs()
  
  melt_FLS<- reshape::melt(FLsdata, id=c("informant","sp_name")) %>% 
    dplyr::filter(value >=1) %>% 
    dplyr::select(-informant) 
  
#Ip <- number who cited for same major purpose (UR in highest use category (-ies))
Ip <- melt_FLS %>% 
   dplyr::group_by(sp_name, variable) %>%
   dplyr::summarize(Ip = sum(value, na.rm = TRUE)) %>% 
   dplyr::slice(which.max(Ip))
  
            
#Bind Ip and Iu data
FLspdata <- dplyr::left_join(Iu, Ip, by = "sp_name", na.rm = TRUE)
 
 #Calculate FLs = Ip *100 / Iu
 FLspdata$FLs <- FLspdata$Ip * 100 / FLspdata$FCs 
  
 FLs <- FLspdata %>% dplyr::group_by(sp_name) %>%
   dplyr::rename(Primary.use = variable) %>%
   dplyr::select(-FCs, -Ip) %>%
   dplyr::arrange(-FLs)%>%
   dplyr::mutate(FLs = round(FLs, 3))
  
  as.data.frame(FLs)
}
