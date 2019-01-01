#' Create a chord diagram of ethnobotany uses and species
#'
#' This function allows you to create a chord diagram of species and uses for ethnobotany studies.
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' 
#' @importFrom magrittr %>%
#' @importFrom reshape melt
#' @importFrom dplyr filter 
#' @importFrom dplyr select 
#' @importFrom circlize chordDiagram
#' 
#' @keywords ethnobotany, cultural value, use report
#'
#' @examples
#' 
#' ethnoChord(ethnobotanydata)
#' 
#' @export ethnoChord
ethnoChord <- function(data) {
    if (!requireNamespace("reshape", quietly = TRUE)) {
        stop("Package \"reshape\" needed for this function to work. Please install it.",
            call. = FALSE)
    }
   if (!requireNamespace("circlize", quietly = TRUE)) {
     stop("Package \"circlize\" needed for this function to work. Please install it.",
          call. = FALSE)
   }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package \"dplyr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  #Melt ethnobotany data
  ethnoChord <- reshape::melt(data, id=c("informant","sp_name")) %>% dplyr::filter(value >=1) %>% dplyr::select(2:3) %>% circlize::chordDiagram(transparency = 0.5)
  
    print("Chord diagram for each use related to each species in the data set")
}
