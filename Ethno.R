#Quantitative Ethnobotany
#“quantitative ethnobotany” coined by Prance et al. (1987)
setwd("/Users/macbook/Dropbox/Contributions/R")


#install.packages("devtools")
library(devtools)
#install latest version from Hadley Wickham
#devtools::install_github("hadley/devtools")
#devtools::install_github("klutometis/roxygen")
library(roxygen2)

usethis::create_package("ethnobotanyR")

setwd("./ethnobotanyR")
document()

build()

ethnobotanydata <-read.csv("ethnobotanydata.csv")
#Example data has 3 plants, 10 interviewees, 10 categories
#data should be rows of species names and columns of use categories
#populated with counts of uses per person (should be 0 or 1 values)
#(max value =N; total number of people interviewed)

#Sum of all UR for all species=====
#test
sum(ethnobotanydata[,-c(1:2)])
#function
URsum<-function(data){
sum(data[, -c(1:2)])
}

URsum(ethnobotanydata)

#Use Reports (UR) per Species=====
  #example
URstestdata<-ethnobotanydata
URstestdata$URps<-rowSums(URstestdata[, -c(1:2)])
library(plyr)
ddply(URstestdata, ~sp_name, summarise, URs = sum(URps))
  #alternative example
  #aggregate(URstestdata$URps, by=list(URs=URstestdata$sp_name), FUN=sum)

#function
URs<-function(data){

  data$URps<-rowSums(data[, -c(1:2)])
  library(plyr)
  data_URs <- ddply(data, ~sp_name, summarise, URs = sum(URps))

  print(data_URs)
}

URs(ethnobotanydata)

#Cultural Importance index (CI)=====

CIs<-function(data){
  data$URps<-rowSums(data[, -c(1:2)])
  library(plyr)
  data_URs <- ddply(data, ~sp_name, summarise, URs = sum(URps))
  data_Ci<-data_URs
  data_Ci$Ci<-data_URs$URs/(length(unique(data$informant))*ncol(data[, -c(1:2)]))
print(data_Ci[c(1,3)])
}

CIs(ethnobotanydata) #maximum CI is N

#use value (UV) index per species=====
#sum of number of different uses mentioned by each informant / n)
#(simplified by Rossato et al. 1999 and Albuquerque et al. 2006)

#test
UVstestdata<-ethnobotanydata
UVstestdata$UVps<-rowSums((UVstestdata[, -c(1:2)]) > 0 )
ddply(UVstestdata, ~sp_name, summarise, UVs = sum(UVps)/(length(unique(UVstestdata$informant))))

#function
UVs<-function(data){
      data$UVps<-rowSums((data[, -c(1:2)]) > 0 )
      ddply(data, ~sp_name, summarise, UVs = sum(UVps)/(length(unique(informant))))
}

UVs(ethnobotanydata)

#number of uses (NU)====

#test
NUdata<-ethnobotanydata
NUdataaggr<-aggregate(NUdata[, -c(1:2)], by=list(sp_name=NUdata$sp_name), FUN=sum)
NUdataaggr[, -1][NUdataaggr[, -1]>0] <- 1
NUdataaggr$NUs<-rowSums(NUdataaggr[, -1])
NUdataaggr[ , c(1, length( names( NUdataaggr ) ) ) ]

#function
NUs<-function(data){
  NUdataaggr<-aggregate(data[, -c(1:2)], by=list(sp_name=data$sp_name), FUN=sum)
  NUdataaggr[, -1][NUdataaggr[, -1]>0] <- 1
  NUdataaggr$NUs<-rowSums(NUdataaggr[, -1])
  print(NUdataaggr[ , c(1, length( names( NUdataaggr ) ) ) ])
}

NUs(ethnobotanydata)

#Frequency of Citation (FC) per species=====
#test
FCstestdata<-ethnobotanydata
FCstestdata$FCps<-rowSums((FCstestdata[, -c(1:2)]) > 0 )
FCstestdata$FCps[FCstestdata$FCps>0] <- 1
ddply(FCstestdata, ~sp_name, summarise, FCs = sum(FCps))

#function
FCs<-function(data){
  data$FCps<-rowSums((data[, -c(1:2)]) > 0 )
  data$FCps[data$FCps>0] <- 1
  ddply(data, ~sp_name, summarise, FCs = sum(FCps))
}

FCs(ethnobotanydata)

#Relative Frequency of Citation (RFC) Pardo-de-Santayana (2003)=====

RFCs<-function(data){
  data$FCps<-rowSums((data[, -c(1:2)]) > 0 )
  data$FCps[data$FCps>0] <- 1
  ddply(data, ~sp_name, summarise, RFCs = sum(FCps)/(length(unique(informant))))
}

RFCs(ethnobotanydata)


#Relative Importance Index (RI) Pardo-de-Santayana (2003)=====
#test
RFCstestdata<-ethnobotanydata
RFCstestdata$FCps<-rowSums((RFCstestdata[, -c(1:2)]) > 0 )
RFCstestdata$FCps[RFCstestdata$FCps>0] <- 1
RFCstestdata2<-ddply(RFCstestdata, ~sp_name, summarise, FCs = sum(FCps))
RFCstestdata2$RFCs<-RFCstestdata2$FCs/max(RFCstestdata2$FCs)
RFCs<-RFCstestdata2[ , c(1, length( names( RFCstestdata2 ) ) ) ]

RNUstestdata<-ethnobotanydata
RNUsdataaggr<-aggregate(RNUstestdata[, -c(1:2)], by=list(sp_name=RNUstestdata$sp_name), FUN=sum)
RNUsdataaggr[, -1][RNUsdataaggr[, -1]>0] <- 1
RNUsdataaggr$NUs<-rowSums(RNUsdataaggr[, -1])
RNUsdataaggr[ , c(1, length( names( RNUsdataaggr ) ) ) ]
RNUsdataaggr$RNUs<-RNUsdataaggr$NUs/max(RNUsdataaggr$NUs)
RNUs<-RNUsdataaggr[ , c(1, length( names( RNUsdataaggr ) ) ) ]

RIs <- merge(RNUs,RFCs,by="sp_name")
RIs$RIs<-(RIs$RNUs + RIs$RFCs)/2

#function
RIs<-function(data){
  RFCstestdata<-data
  RFCstestdata$FCps<-rowSums((RFCstestdata[, -c(1:2)]) > 0 )
  RFCstestdata$FCps[RFCstestdata$FCps>0] <- 1
  RFCstestdata2<-ddply(RFCstestdata, ~sp_name, summarise, FCs = sum(FCps))
  RFCstestdata2$RFCs<-RFCstestdata2$FCs/max(RFCstestdata2$FCs)
  RFCs<-RFCstestdata2[ , c(1, length( names( RFCstestdata2 ) ) ) ]

  RNUstestdata<-data
  RNUsdataaggr<-aggregate(RNUstestdata[, -c(1:2)], by=list(sp_name=RNUstestdata$sp_name), FUN=sum)
  RNUsdataaggr[, -1][RNUsdataaggr[, -1]>0] <- 1
  RNUsdataaggr$NUs<-rowSums(RNUsdataaggr[, -1])
  RNUsdataaggr[ , c(1, length( names( RNUsdataaggr ) ) ) ]
  RNUsdataaggr$RNUs<-RNUsdataaggr$NUs/max(RNUsdataaggr$NUs)
  RNUs<-RNUsdataaggr[ , c(1, length( names( RNUsdataaggr ) ) ) ]

  RIs <- merge(RNUs,RFCs,by="sp_name")
  RIs$RIs<-(RIs$RNUs + RIs$RFCs)/2
  print(RIs[ , c(1, length( names( RIs ) ) ) ])
}

RIs(ethnobotanydata)

#Cultural Value Index (CV) Reyes-García et al. (2006)====


CVNUdata<-ethnobotanydata
CVNUdataaggr<-aggregate(CVNUdata[, -c(1:2)], by=list(sp_name=CVNUdata$sp_name), FUN=sum)
CVNUdataaggr[, -1][CVNUdataaggr[, -1]>0] <- 1
CVNUdataaggr$NUs<-rowSums(CVNUdataaggr[, -1])
CVNUdataaggr$NUdivNC <- CVNUdataaggr$NUs/ncol(CVNUdata[, -c(1:2)])
NUdivNC <- CVNUdataaggr[ , c(1, length( names( CVNUdataaggr ) ) ) ]

CVFCsdata<-ethnobotanydata
CVFCsdata$FCps<-rowSums((CVFCsdata[, -c(1:2)]) > 0 )
CVFCsdata$FCps[CVFCsdata$FCps>0] <- 1
CVFCdata2<-ddply(CVFCsdata, ~sp_name, summarise, FCs = sum(FCps))
CVFCdata2$FCdivN<-CVFCdata2$FCs/(length(unique(CVFCsdata$informant)))
FCdivN<-CVFCdata2[ , c(1, length( names( CVFCdata2 ) ) ) ]


CVURdata<-ethnobotanydata
CVURdata$URps<-rowSums(CVURdata[, -c(1:2)])
library(plyr)
CVURdata2 <- ddply(CVURdata, ~sp_name, summarise, URs = sum(URps))
CVURdata2$URdivN<-CVURdata2$URs/(length(unique(CVURdata$informant)))
URdivN <- CVURdata2[ , c(1, length( names( CVURdata2 ) ) ) ]

CVs1 <- merge(NUdivNC, FCdivN, by="sp_name")
CVs2 <- merge(CVs1, URdivN, by="sp_name")
CVs2$CVs<-CVs2$NUdivNC * CVs2$FCdivN * CVs2$URdivN

#function
CVs<-function(data){
  if (!requireNamespace("plyr", quietly = TRUE)) {
    stop("Package \"plyr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  CVURdata<-data
  CVURdata$URps<-rowSums(CVURdata[, -c(1:2)])
  library(plyr)
  CVURdata2 <- ddply(CVURdata, ~sp_name, summarise, URs = sum(URps))
  CVURdata2$URdivN<-CVURdata2$URs/(length(unique(CVURdata$informant)))
  URdivN <- CVURdata2[ , c(1, length( names( CVURdata2 ) ) ) ]

  CVs1 <- merge(NUdivNC, FCdivN, by="sp_name")
  CVs2 <- merge(CVs1, URdivN, by="sp_name")
  CVs2$CVs<-CVs2$NUdivNC * CVs2$FCdivN * CVs2$URdivN
  print(CVs2[ , c(1, length( names( CVs2 ) ) ) ])
}

CVs(ethnobotanydata)

#Create a package====
setwd(".")
document()

setwd("..")
install("ethnobotanyR")

