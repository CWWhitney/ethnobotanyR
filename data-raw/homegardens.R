# Builds data/homegardens.rda, homegardens_info.rda and homegardens_species.rda from the
# original survey data, with the filters of the published analysis
# (Whitney et al. 2018 doi:10.14237/ebl.9.2.2018.503; code 'Quantitavie Ethno Code.R').
# Survey: Whitney et al. 2017 doi:10.1016/j.agsy.2017.03.009
# Source: https://github.com/CWWhitney/Uganda_Homegarden_Agrobiodiv (data/)
src <- "~/Library/CloudStorage/Dropbox/Contributions/Completed/15_Uganda_Homegardens/Uganda_Homegarden_Agrobiodiv/data/"
raw <- read.csv(file.path(src, "AllRawNoWeeds_v7.csv"))

# As published: first-visit count above 0 and identified plants only
keep <- raw[which(raw$count1stVisit_numberround_ > "0" & raw$FullDescription > "0"), ]

# 14 use categories (Table 1 order); the 'weed' category is excluded as in the paper
use_in <- c("URFood", "URSale", "URMed_Spirit", "URTech", "UROrnament", "URFence", "URFirewood",
            "URTimber", "URShade_Shelter_Wind", "URHygeine", "URAnimalFeed", "URShare",
            "URPesticide", "URManure")
use_out <- c("food", "sale", "medicine", "technical", "ornament", "fence", "firewood",
             "timber", "shade", "hygiene", "animal_feed", "share", "pesticide", "manure")

# Species: genus + epithet (or Musa group) from BotanicalName, authorities dropped
clean_name <- function(x) {
  x <- gsub("\\bcf\\.\\s*", "", trimws(x))
  tok <- strsplit(x, "\\s+")[[1]]
  if (length(tok) == 1) return(paste(tok, "sp."))
  if (tok[1] == "Musa" && grepl("^\\(", tok[2])) {
    close <- which(grepl("\\)$", tok[-1]))[1] + 1
    return(paste(tok[1:close], collapse = " "))
  }
  if (grepl("^[a-z]", tok[2]) || tok[2] %in% c("sp.", "spp.")) return(paste(tok[1:2], collapse = " "))
  paste(tok[1], "sp.")
}
sp_name <- vapply(keep$BotanicalName, clean_name, character(1))

homegardens <- data.frame(informant = keep$Garden, sp_name = sp_name, keep[use_in])
names(homegardens)[-(1:2)] <- use_out
homegardens[use_out] <- lapply(homegardens[use_out], as.integer)
homegardens <- homegardens[order(homegardens$informant, homegardens$sp_name), ]
rownames(homegardens) <- NULL
homegardens$informant <- factor(homegardens$informant)
homegardens$sp_name <- factor(homegardens$sp_name)

stopifnot(!anyNA(homegardens), all(unlist(homegardens[use_out]) %in% 0:1),
          sum(homegardens[use_out]) == 3961, nlevels(homegardens$sp_name) == length(unique(keep$BotanicalName)),
          nlevels(homegardens$informant) == 102)
save(homegardens, file = "data/homegardens.rda", compress = "xz")

# Garden covariates (no coordinates, names or household characteristics are included)
gs <- read.csv(file.path(src, "GardenStats.csv"))
gs <- gs[match(levels(homegardens$informant), gs$Garden), ]
homegardens_info <- data.frame(informant = factor(gs$Garden, levels = levels(homegardens$informant)),
                               district = factor(gs$District), village = factor(gs$Village),
                               altitude_m = gs$Altitudem, area_m2 = gs$Aream2,
                               nearest_market_km = gs$NearestMarketkm,
                               travel_time_market_h = gs$Traveltimetomarkethr)
stopifnot(!anyNA(homegardens_info), nrow(homegardens_info) == 102)
save(homegardens_info, file = "data/homegardens_info.rda", compress = "xz")

# Species traits: life form, family and whether native, as recorded. Species with more than one
# recorded value take the most frequent; ties take the first, except three listed family typos.
keep$sp_name <- sp_name
mode1 <- function(x) names(sort(table(x), decreasing = TRUE))[1]
sp <- data.frame(sp_name = sort(unique(sp_name)))
sp$type <- tapply(keep$Type, keep$sp_name, mode1)[sp$sp_name]
sp$family <- tapply(keep$Family, keep$sp_name, mode1)[sp$sp_name]
sp$family[sp$sp_name == "Callistemon citrinus"] <- "Myrtaceae"
sp$family[sp$sp_name == "Crassocephalum mannii"] <- "Asteraceae"
sp$family[sp$sp_name == "Sambucus africana"] <- "Caprifoliaceae"
nat <- tapply(keep$nativey1, keep$sp_name, function(x) mean(x, na.rm = TRUE))[sp$sp_name]
sp$native <- ifelse(nat %in% c(0, 1), as.integer(nat), NA_integer_)  # NA where records disagree
homegardens_species <- data.frame(sp_name = factor(sp$sp_name, levels = levels(homegardens$sp_name)),
                                  type = factor(sp$type), family = factor(sp$family), native = sp$native)
rownames(homegardens_species) <- NULL
stopifnot(nrow(homegardens_species) == 225, !anyNA(homegardens_species$sp_name))
save(homegardens_species, file = "data/homegardens_species.rda", compress = "xz")
