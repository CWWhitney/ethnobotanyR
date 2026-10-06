#' Homegarden ethnobotany data from southwestern Uganda.
#'
#' Plants recorded in 102 homegardens in southwestern Uganda, with 14 use categories. Each informant is one family's garden. Same format as \code{ethnobotanydata}. This is the data set analysed in Whitney et al. (2018); see also Whitney et al. (2017).
#'
#' One row is one species in one garden. Gardens only have rows for species they grow, so the number of informants differs by species: \code{ethno_beta()} estimates the share of gardens growing a species that use it. Use columns are 0/1 (1 = the species is used for that purpose in that garden); one species can have several uses. The data match the published analysis: 3,961 use reports, 225 species, 102 gardens. Only plants recorded on the first visit and identified were kept (as in the paper); the 'weed' category is excluded. Species names are as recorded in the survey, reduced to genus and epithet (or Musa group). Some uses are rare (for example, manure has 2 use reports); drop or merge sparse categories before modeling.
#'
#' @keywords misc survey
#'
#' @docType data
#'
#' @format A data frame with 2870 rows and 16 variables:
#' \describe{
#'   \item{informant}{garden id (102 gardens); letters are the survey site, number is the garden}
#'   \item{sp_name}{225 species names}
#'   \item{food, sale, medicine, technical, ornament, fence, firewood, timber, shade, hygiene, animal_feed, share, pesticide, manure}{use categories, 0 = not used, 1 = used}
#' }
#'
#' @source Original survey data: \url{https://github.com/CWWhitney/Uganda_Homegarden_Agrobiodiv} (\code{data/AllRawNoWeeds_v7.csv}, \code{data/GardenStats.csv}). Rebuilt by \code{data-raw/homegardens.R} in the package source.
#'
#' @seealso \code{\link{homegardens_info}}, \code{\link{homegardens_species}}
#'
#' @references
#' Whitney C, Tabuti JRS, Hensel O, Yeh C, Gebauer J, Luedeling E (2017). Homegardens and the future of food and nutrition security in southwest Uganda. Agricultural Systems, 154, 133-144. \doi{10.1016/j.agsy.2017.03.009}
#'
#' Whitney C, Bahati J, Gebauer J (2018). Ethnobotany and agrobiodiversity: valuation of plants in the homegardens of southwestern Uganda. Ethnobiology Letters, 9(2), 90-100. \doi{10.14237/ebl.9.2.2018.503}
#'
#' @examples
#' # share of gardens growing a species that report each use
#' head(ethno_beta(homegardens[homegardens$sp_name == "Persea americana", ]))
#'
"homegardens"

#' Garden covariates for the homegarden data.
#'
#' One row per garden in \code{homegardens}, for modeling use by garden. Coordinates, informant names and household characteristics of the survey are not included.
#'
#' @keywords misc survey
#' @docType data
#'
#' @format A data frame with 102 rows and 7 variables:
#' \describe{
#'   \item{informant}{garden id, matches \code{homegardens$informant}}
#'   \item{district}{Bushenyi, Rubirizi or Sheema}
#'   \item{village}{survey village (9)}
#'   \item{altitude_m}{altitude in metres}
#'   \item{area_m2}{garden area in square metres}
#'   \item{nearest_market_km}{distance to the nearest market in km}
#'   \item{travel_time_market_h}{travel time to the nearest market in hours}
#' }
#'
#' @source \url{https://github.com/CWWhitney/Uganda_Homegarden_Agrobiodiv} (\code{data/GardenStats.csv}).
#'
#' @references
#' Whitney C, Tabuti JRS, Hensel O, Yeh C, Gebauer J, Luedeling E (2017). Homegardens and the future of food and nutrition security in southwest Uganda. Agricultural Systems, 154, 133-144. \doi{10.1016/j.agsy.2017.03.009}
#'
#' @seealso \code{\link{homegardens}}
#'
"homegardens_info"

#' Species traits for the homegarden data.
#'
#' One row per species in \code{homegardens}, as recorded in the survey. Species with conflicting records take the most frequent value; three family typos were resolved by hand (see \code{data-raw/homegardens.R}).
#'
#' @keywords misc survey
#' @docType data
#'
#' @format A data frame with 225 rows and 4 variables:
#' \describe{
#'   \item{sp_name}{species, matches \code{homegardens$sp_name}}
#'   \item{type}{life form: annual forb, annual grass, palm, perennial forb, perennial grass, shrub, tree or vine}
#'   \item{family}{plant family as recorded}
#'   \item{native}{1 = recorded as native, 0 = recorded as not native}
#' }
#'
#' @source \url{https://github.com/CWWhitney/Uganda_Homegarden_Agrobiodiv} (\code{data/AllRawNoWeeds_v7.csv}).
#'
#' @references
#' Whitney C, Bahati J, Gebauer J (2018). Ethnobotany and agrobiodiversity: valuation of plants in the homegardens of southwestern Uganda. Ethnobiology Letters, 9(2), 90-100. \doi{10.14237/ebl.9.2.2018.503}
#'
#' @seealso \code{\link{homegardens}}
#'
"homegardens_species"
