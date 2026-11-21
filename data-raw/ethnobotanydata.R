# Renames the long-format identifier column of the built-in ethnobotanydata
# example from 'informant' to 'participant' (0.4.0 terminology change), keeping
# all observations, species, uses and row order unchanged.
load("data/ethnobotanydata.rda")

names(ethnobotanydata)[names(ethnobotanydata) == "informant"] <- "participant"
levels(ethnobotanydata$participant) <- sub("^inform_", "participant_", levels(ethnobotanydata$participant))

stopifnot(
  dim(ethnobotanydata) == c(80, 12),
  nlevels(ethnobotanydata$participant) == 20,
  nlevels(ethnobotanydata$sp_name) == 4
)

save(ethnobotanydata, file = "data/ethnobotanydata.rda", compress = "bzip2", version = 2)
