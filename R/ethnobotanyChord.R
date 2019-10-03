#' Chord diagram of informants and species uses
#'
#' Creates a chord diagram of informants and species uses for ethnobotany studies. For more on the circlize package see Zuguang Gu's 'Circular Visualization in R' <https://jokergoo.github.io/circlize_book/book/>
#' @source Whitney, Cory W., Joseph Bahati, and J. Gebauer. 2018. “Ethnobotany and Agrobiodiversity; Valuation of Plants in the Homegardens of Southwestern Uganda.” Ethnobiology Letters 9 (2): 90–100. 
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' 
#' @keywords internal
#' 
#' #' @return Chord diagram figure for each use per 'sp_name' (top half) related to each 'informant' (bottom half) in the data set. 
#' To change variable names try using the dplyr rename function.
#' 
#' @section Warning:
#' 
#' ethnobotanyChord is deprecated. Use ethnoChord() 
#' and ethnoChordUser() functions instead.
#' 
#' @importFrom circlize chordDiagram  circos.text  get.cell.meta.data
#' @importFrom dplyr filter select 
#' @importFrom graphics strwidth
#' @importFrom magrittr %>%
#' @importFrom reshape melt
#' 
#' 
#' @examples
#' 
#' #Use built-in ethnobotany data example
#' ethnobotanyChord(ethnobotanydata)
#' 
#' #Generate random dataset of three informants uses for four species
#' 
#' eb_data <- data.frame(replicate(10,sample(0:1,20,rep=TRUE)))
#' names(eb_data) <- gsub(x = names(eb_data), pattern = "X", replacement = "Use_")  
#' eb_data$informant <- sample(c('User_1', 'User_2', 'User_3'), 20, replace=TRUE)
#' eb_data$sp_name <- sample(c('sp_1', 'sp_2', 'sp_3', 'sp_4'), 20, replace=TRUE)
#' 
#' ethnobotanyChord(eb_data)
#' 
#' @export ethnobotanyChord
ethnobotanyChord <- function(data) {
  .Deprecated("ethnoChord")  
  "ethnobotanyChord is deprecated. Use ethnoChord() and ethnoChordUser() functions instead."
}
