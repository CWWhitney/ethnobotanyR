#Quantitative Ethnobotany
#“quantitative ethnobotany” coined by Prance et al. (1987)
setwd("/Users/macbook/Dropbox/Contributions/R/ethnobotanyR")


#install.packages("devtools")
library(devtools)
#install latest version from Hadley Wickham
devtools::install_github("devtools" , "hadley")
library(roxygen2)
create("ethnobotanyR")
