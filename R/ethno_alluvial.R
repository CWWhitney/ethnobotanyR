#' Alluvial plot of ethnobotany uses and species
#'
#' Creates a simple alluvial plot of species and uses for ethnobotany studies. 
#' @usage ethno_alluvial(data)
#' 
#' 
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' 
#' @keywords graphs arith math logic methods misc survey
#' 
#' @return Alluvial diagram figure for each use (top half) related to each 'sp_name' (bottom half) in the data set. 
#' To change variable names try using the dplyr rename function.
#' 
#' @section Warning:
#' 
#' Identification for informants and species must be listed by the names 'informant' and 'sp_name' respectively in the data set.
#' The rest of the columns represent the identified ethnobotany use categories. These data should be populated with counts of uses per person (should be 0 or 1 values).
#' 
#' @importFrom dplyr filter rename select 
#' @importFrom magrittr %>%
#' @importFrom reshape melt
#' @importFrom ggalluvial StatStratum geom_stratum geom_alluvium
#' @importFrom ggplot2 aes ggplot geom_text scale_x_continuous coord_flip 
#'
#' @examples
#' 
#' #Use built-in ethnobotany data example
#' ethno_alluvial(ethnobotanydata)
#' 
#' #Generate random dataset of three informants uses for four species
#' 
#' eb_data <- data.frame(replicate(10,sample(0:1,20,rep=TRUE)))
#' names(eb_data) <- gsub(x = names(eb_data), pattern = "X", replacement = "Use_")  
#' eb_data$informant <- sample(c('User_1', 'User_2', 'User_3'), 20, replace=TRUE)
#' eb_data$sp_name <- sample(c('sp_1', 'sp_2', 'sp_3', 'sp_4'), 20, replace=TRUE)
#' 
#' ethno_alluvial(eb_data)
#' 
#' @export ethno_alluvial
#' 
ethno_alluvial <- function(data) {
  
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
    
    if (!requireNamespace("ggalluvial", quietly = TRUE)) {
      stop("Package \"ggalluvial\" needed for this function to work. Please install it.",
           call. = FALSE)
    }
    

    if (!requireNamespace("magrittr", quietly = TRUE)) {
      stop("Package \"magrittr\" needed for this function to work. Please install it.",
           call. = FALSE)
    }    
  }# end package loading check
  
  # Use 'complete.cases' from stats to get to the collection of obs without NA
  
  if (any(is.na(data))) {
    warning("Some of your observations included \"NA\" and were removed. Consider using \"0\" instead.")
    data<-data[stats::complete.cases(data), ]
  }# end error stops
  
  # Set the variables to NULL first, appeasing R CMD check
  informant <- sp_name <- variable <- value <- strwidth <- NULL 
  
  # Melt ethnobotany data
  mat <- reshape::melt(data, id=c("informant","sp_name")) %>% 
    dplyr::filter(value >=1)%>%
   # dplyr::rename("use" = "variable") %>%  
    dplyr::arrange(variable) 
  
  # Create alluvial plot ####
  
  StatStratum <- ggalluvial::StatStratum
  
  ggplot2::ggplot(as.data.frame(mat),
         aes(
           y = value,
           axis1 = sp_name,
           axis3 = variable
         )) +
    ggalluvial::geom_alluvium(
      aes(fill = sp_name),
      width = 0,
      knot.pos = 0,
      reverse = FALSE
    ) +
    ggplot2::guides(fill = FALSE) +
    ggalluvial::geom_stratum(width = 1 / 8, reverse = FALSE) +
    ggplot2::geom_text(stat = "stratum",
              infer.label = TRUE,
              reverse = FALSE) +
    ggplot2::scale_x_continuous(breaks = 1:2,
                       labels = c("sp_name", "use")) +
    ggplot2::theme_minimal() +
    ggplot2::coord_flip() 
  
}
