#' Bootstrap analyses of ethnobotany indices
#'
#' Creates a non-parametric bootstrap as a Bayesian Model \url{http://www.sumsar.net/blog/2015/04/the-non-parametric-bootstrap-as-a-bayesian-model/}. This is meant to be applied for ethnobotany data and indices in the ethnobotanyR package. Performs a Bayesian bootstrap and returns a sample of size 'n1' representing the posterior distribution of the chosen statistic (i.e. 'mean'). The function returns a vector if the statistic is one-dimensional (like for mean(...)) or a data.frame if the statistic is multi-dimensional (like for the coefficients 'coefs.' of a regression model 'lm').
#' 
#' @references 
#' Bååth, Rasmus. “The Non-Parametric Bootstrap as a Bayesian Model” Publishable Stuff, 2015. \url{http://www.sumsar.net/blog/2015/04/the-non-parametric-bootstrap-as-a-bayesian-model/}.
#' Rubin, Donald B. “The Bayesian Bootstrap.” Annals of Statistics 9, no. 1 (January 1981): 130–34. \doi{10.1214/aos/1176345338}.
#' 
#' @usage ethno_boot(data, statistic, n1 = 1000, 
#' n2 = 1000, ...)
#' 
#' @param data Can be either a vector, matrix or a data.frame.
#' @param statistic A function that accepts data as its first argument. Should return a numeric vector.
#' @param n1 The size of the bootstrap sample.
#' @param n2 The sample size used to calculate the statistic for each bootstrap draw.
#' @param ... Further arguments passed on to the statistic function.
#' 
#' @keywords Bayes Bayesian graphs arith math logic methods misc survey
#'
#' @return Bayesian bootstrap of chosen ethnobotany indices in ethnobotanyR package.
#' 
#' @section Application:
#' 
#'  This function was inspired by Rasmus Bååth's “The Non-Parametric Bootstrap as a Bayesian Model” Publishable Stuff, 2015. \url{http://www.sumsar.net/blog/2015/04/the-non-parametric-bootstrap-as-a-bayesian-model/}.
#'  In order to understand the various possible applications of this function for ethnobotany analyses it is important to read through that work. 
#' 
#'  ethnobotanyR users often have a large number of counts in cells of the data set after categorization (i.e one user cites ten different ‘food’ uses but this is just one category). 
#'  Most quantitative ethnobotany tools are not equipped for cases where the theoretical maximum number of use reports in one category, for one species by one informant is >1. 
#'  This function and the ethno_bayes_consensus function may be useful to work with these richer datasets for the Bayes consensus analysis.
#'  
#' @importFrom stats rexp complete.cases
#'  
#' @examples
#' 
#' #Use built-in ethnobotany data example
#' 
#' ethno_boot(data = ethnobotanydata$Use_1, 
#' statistic = mean, n1 = 1000)
#' 
#' #Generate random dataset of three informants uses for four species
#' 
#' eb_data <- data.frame(replicate(10,sample(0:1,20,rep=TRUE)))
#' names(eb_data) <- gsub(x = names(eb_data), pattern = "X", replacement = "Use_")  
#' eb_data$informant<-sample(c('User_1', 'User_2', 'User_3'), 20, replace=TRUE)
#' eb_data$sp_name<-sample(c('sp_1', 'sp_2', 'sp_3', 'sp_4'), 20, replace=TRUE)
#' 
#' # Simple bootstrap of the mean ####
#' 
#' boot_dataUR <- URs(eb_data)
#' 
#' ethno_boot(data = boot_dataUR$URs, statistic = mean)
#' 
#' @export ethno_boot
#' 
ethno_boot <- function(data, statistic, 
                       n1 = 1000, 
                       n2 = 1000, 
                         ...) {
  
  ## Use 'complete.cases' from stats to get to the collection of obs without NA
  if (any(is.na(data))) {
    warning("Some of your observations included \"NA\" and were removed. Consider using \"0\" instead.")
    data<-data[stats::complete.cases(data), ]
  }#end error stops
  
  # Set the variables to NULL first, appeasing R CMD check
  value <-  meltURdata <- URdata <- URs <- sp_name <- informant <- URps <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
  #Bayes boot
  
    # Draw from a uniform Dirichlet distribution with alpha set to rep(1, n_dim).
    # Using the facts that you can transform gamma distributed draws into 
    # Dirichlet draws and that rgamma(n, 1) <=> rexp(n, 1)
    dirichlet_weights <- matrix(stats::rexp(NROW(data) * n1, 1) , 
                                 ncol = NROW(data), byrow = TRUE)
    dirichlet_weights <- dirichlet_weights / 
      rowSums(dirichlet_weights)
    
      if(is.null(dim(data)) || length(dim(data)) < 2) { 
        # data is a list type of object
        boot_sample <- apply(dirichlet_weights, 1, function(w) {
          data_sample <- sample(data, size = n2, 
                                replace = TRUE, prob = w)
          statistic(data_sample, ...)
        })
      } else { # data is a table type of object
        boot_sample <- apply(dirichlet_weights, 1, function(w) {
          index_sample <- sample(nrow(data), 
                                 size = n2, replace = TRUE, prob = w)
          statistic(data[index_sample, ,drop = FALSE], ...)
        })
      }
    
    if(is.null(dim(boot_sample)) || length(dim(boot_sample)) < 2) {
      # If the bootstrap sample is just a simple vector return it.
      boot_sample
    } else {
      # Otherwise it is a matrix. Since apply returns one row per statistic
      # transpose it and return it as a data frame.
      as.data.frame(t(boot_sample))
    }
  }
  
