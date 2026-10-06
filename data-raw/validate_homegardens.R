# Compares homegardens with the published totals (Whitney et al. 2018, Tables 1-2, doi:10.14237/ebl.9.2.2018.503)
suppressMessages(devtools::load_all(quiet = TRUE))
d <- homegardens
use <- setdiff(names(d), c("informant", "sp_name"))

table1 <- data.frame(
  use = c("food", "sale", "medicine", "technical", "ornament", "fence", "firewood",
          "timber", "shade", "hygiene", "animal_feed", "share", "pesticide", "manure"),
  paper_UR = c(2145, 604, 426, 267, 150, 117, 99, 44, 34, 33, 23, 13, 4, 2),
  paper_species = c(136, 67, 142, 74, 51, 21, 35, 20, 25, 19, 11, 11, 4, 2))
table1$data_UR <- colSums(d[table1$use])
table1$data_species <- sapply(table1$use, function(u) length(unique(d$sp_name[d[[u]] == 1])))
table1$diff_UR <- table1$data_UR - table1$paper_UR
print(table1)
cat("Total UR  paper 3961, data", sum(table1$data_UR), "\n")
cat("Species   paper 225, data", nlevels(droplevels(d$sp_name)), "(identified to species:",
    length(unique(grep(" sp\\.$", d$sp_name, invert = TRUE, value = TRUE))), ")\n")
cat("Gardens   paper 102, data", nlevels(d$informant), "\n")

table2 <- c("Musa (AAA-EAHB Group)" = 169, "Musa (AB Group)" = 134, "Musa (AAA Group)" = 131,
  "Draceana fragrans" = 120, "Persea americana" = 120, "Musa (AAB Group)" = 116,
  "Coffea canephora" = 99, "Xanthosoma sagittifolium" = 95, "Psidium guajava" = 88,
  "Saccharum officinarum" = 88, "Manihot esculenta" = 81, "Mangifera indica" = 80,
  "Phaseolus vulgaris" = 79, "Carica papaya" = 73, "Cucurbita pepo" = 71,
  "Artocarpus heterophyllus" = 71, "Solanum aethiopicum" = 69, "Solanum anguivi" = 65,
  "Passiflora edulis" = 65, "Solanum lycopersicum" = 61, "Ananas comosus" = 58,
  "Eucalyptus grandis" = 58, "Eriobotrya japonica" = 55, "Physalis peruviana" = 55,
  "Capsicum frutescens" = 54, "Amaranthus hybridus" = 52, "Euphorbia tirucalli" = 51,
  "Coffea arabica" = 49, "Musa (ABB Group)" = 48, "Amaranthus dubius" = 45)
ur <- URs(d)
t2 <- data.frame(species = names(table2), paper_UR = as.numeric(table2),
                 data_UR = ur$URs[match(names(table2), ur$sp_name)])
t2$diff <- t2$data_UR - t2$paper_UR
print(t2)
cat("Table 2: exact", sum(t2$diff == 0), "| within 3:", sum(abs(t2$diff) <= 3),
    "| of 30; correlation", round(cor(t2$paper_UR, t2$data_UR), 3), "\n")
