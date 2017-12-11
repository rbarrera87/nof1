#' Make a network object containing data, priors, and a jags model file
#'
#' @param Y Outcome
#' @param Treat Treatment indicator vector
#' @param baseline baseline Treatment name
#' @param ncat Number of categories. Used in ordinal models
#' @param response Type of outcome. Can be normal, binomial, poisson or ordinal
#' @export
 
nof1.data <- function(Y, Treat, baseline = "baseline", ncat = NULL, response = NULL, Time=NULL, knots = NULL,
                      alpha.prior = NULL, beta.prior = NULL, gamma.prior = NULL, dc.prior = NULL, c1.prior = NULL,
                      rho.prior = NULL, hy.prior = NULL){
  
  if(response == "ordinal"){
    if(is.null(ncat)){
      stop("ncat (number of categories) must be entered for ordinal response")
    }
  }
  nobs <- length(Y)
  
  if(!baseline %in% Treat){
    stop("baseline treatment name is not in Treat")
  }
  Treat.name <- unique(Treat)
  Treat.name <- Treat.name[Treat.name != baseline]
  
  nof1 = list(Y = Y, Treat = Treat, baseline = baseline, ncat = ncat, nobs = nobs, Treat.name = Treat.name, response = response)

  if(!is.null(Time)){
    cen.Time <- (Time - mean(Time, na.rm = TRUE)) / sd(Time, na.rm = TRUE)
    nof1$Time = cen.Time
  }

  if(!is.null(knots)){
    cen.knots <- (knots - mean(Time, na.rm = TRUE))/ sd(Time, na.rm = TRUE)
    BS <- bs(cen.Time, knots = cen.knots)
    nof1$BS <- BS
    nof1$knots <- knots
  }

  for(i in Treat.name){
    nam <- paste("Treat_", i, sep = "")
    nam <- assign(nam, as.numeric(Treat == i))
    nof1[[ paste("Treat_", i, sep = "")]] <- nam
  }

  prior.param <- list(response = response, dc.prior = dc.prior, c1.prior = c1.prior, alpha.prior = alpha.prior, beta.prior = beta.prior, gamma.prior = gamma.prior, hy.prior = hy.prior, rho.prior = rho.prior)
  prior.data <- nof1.prior.default(prior.param)

  nof1 <- c(nof1, prior.data)

  code <- nof1.rjags(nof1)
  nof1$code <- code

  class(nof1) <- "nof1.data"
  nof1
}



