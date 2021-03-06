% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/URsum.R
\name{URsum}
\alias{URsum}
\title{Sum of all Use Reports (UR) for all species}
\usage{
URsum(data)
}
\arguments{
\item{data}{is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).}
}
\value{
Single value sum of all ethnobotany use reports (UR) for all species in the data.
}
\description{
Calculates the sum of all ethnobotany use reports (UR) for all species, a common metric for ethnobotany studies.
}
\section{Warning}{


Identification for informants and species must be listed by the names 'informant' and 'sp_name' respectively in the data set.
The rest of the columns should all represent separate identified ethnobotany use categories. These data should be populated with counts of uses per informant (should be 0 or 1 values).
}

\examples{

#Use built-in ethnobotany data example
URsum(ethnobotanydata)

#Generate random dataset of three informants uses for four species

eb_data <- data.frame(replicate(10,sample(0:1,20,rep=TRUE)))
names(eb_data) <- gsub(x = names(eb_data), pattern = "X", replacement = "Use_")  
eb_data$informant <- sample(c('User_1', 'User_2', 'User_3'), 20, replace=TRUE)
eb_data$sp_name <- sample(c('sp_1', 'sp_2', 'sp_3', 'sp_4'), 20, replace=TRUE)

URsum(eb_data)

}
\references{
Prance, G. T., W. Balee, B. M. Boom, and R. L. Carneiro. 1987. “Quantitative Ethnobotany and the Case for Conservation in Amazonia.” Conservation Biology 1 (4): 296–310.
}
\keyword{arith}
\keyword{logic}
\keyword{math}
\keyword{methods}
\keyword{misc}
\keyword{survey}
