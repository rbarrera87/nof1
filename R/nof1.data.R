#' Make a network object containing data, priors, and a jags model file
#'
#' @param Y Outcome
#' @export

nof1.data <- function(Y, Treat, Time=NULL, ncat = NULL, knots = NULL, baseline = "baseline", response = NULL,
                      alpha.prior = NULL, beta.prior = NULL, gamma.prior = NULL, dc.prior = NULL, c1.prior = NULL,
                      rho.prior = NULL, hy.prior = NULL){

  nobs <- length(Y)
  Treat.name <- unique(Treat)
  
  if(!baseline %in% Treat){
    stop("wrong baseline name")
  }
  
  if(response == "ordinal"){
    if(is.null(ncat)){
      stop("ncat (number of categories) must be entered for ordinal response")
    }
  }
  
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



