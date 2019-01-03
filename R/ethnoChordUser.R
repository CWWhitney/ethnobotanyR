#' Create a chord diagram of ethnobotany uses and species
#'
#' This function allows you to create a chord diagram of informants and species uses for ethnobotany studies.
#' @source Whitney, C. W., Bahati, J., and Gebauer, J. (2018), Ethnobotany and agrobiodiversity; valuation of plants in the homegardens of southwestern Uganda. Ethnobiology Letters, 9(2), 90-100. <DOI:10.14237/ebl.9.2.2018.503.>
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' 
#' @importFrom magrittr %>%
#' @importFrom reshape melt
#' @importFrom dplyr filter 
#' @importFrom dplyr select 
#' @importFrom circlize chordDiagram
#' @importFrom circlize circos.text 
#' @importFrom circlize get.cell.meta.data
#' @importFrom graphics strwidth
#' 
#' @keywords ethnobotany, cultural value, use report
#'
#' @examples
#' 
#' ethnoChordUser(ethnobotanydata)
#' 
#' @export ethnoChordUser
ethnoChordUser <- function(data) {
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
  
  value <- strwidth <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
  #Melt ethnobotany data
  mat<- reshape::melt(data, id=c("informant","sp_name")) %>% dplyr::filter(value >=1) %>% dplyr::select(1,3) 
    
  #Create chord plot
  
  circlize::chordDiagram(mat, annotationTrack = "grid", 
                         preAllocateTracks = list(track.height = max(graphics::strwidth(unlist(dimnames(mat))))))
  circlize::circos.track(track.index = 1, panel.fun = function(x, y) {
    xlim = circlize::get.cell.meta.data("xlim")
    xplot = circlize::get.cell.meta.data("xplot")
    ylim = circlize::get.cell.meta.data("ylim")
    sector.name = circlize::get.cell.meta.data("sector.index")
    
    if(abs(xplot[2] - xplot[1]) < 20) {
      circlize::circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                            niceFacing = TRUE, adj = c(0, 0.5), col = "black")
    } else {
      circlize::circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                            niceFacing = TRUE, adj = c(0, 0.5), col = "black")
    }
  }, bg.border = NA)
  
    print("Chord diagram for each use related to each informant in the data set")
}
