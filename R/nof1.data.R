#' Read json data in as an R object
#'
#' @param json.file input data
#' @export

read_input_data <- function(data, metadata){

  if(metadata$user_age < 14){
    Outcome <- data$parent_response
  }else{
    if(sum(is.na(data$parent_response)) < sum(is.na(data$child_response))){
      Outcome <- data$parent_response
    }else{
      Outcome <- data$child_response
    }
  }

  Treatment <- data[,"treatment"]
  Treatment <- unlist(Treatment)
  Treatment[Treatment == "strict"] = "A"
  Treatment[Treatment == "liberalized"] = "B"

  length_each <- sapply(Outcome[,"daily_stool_consistency"], length)
  Treat <- rep(Treatment, times = length_each)

  Treatment_weekly <- Treatment[seq(1, length(Treatment), 7)]

  stool_consistency <- unlist(Outcome$daily_stool_consistency)
  stool_consistency <- ifelse(1 < stool_consistency & stool_consistency < 7, 0, 1)

  stool_frequency <- unlist(Outcome$daily_stool_frequency)
  pain_interference <- as.vector(unlist(Outcome$promis_pain_interference))
  gi_symptoms <- as.vector(unlist(Outcome$promis_gi_symptoms))

  list(Treatment = Treat, Treatment_weekly = Treatment, stool_consistency = stool_consistency, stool_frequency = stool_frequency, pain_interference = pain_interference,  gi_symptoms = gi_symptoms)
}


#' Make a network object containing data, priors, and a jags model file
#'
#' @param Y Outcome
#' @export

nof1.data <- function(Y, Treat, Time=NULL, ncat = NULL, knots = NULL, baseline = "baseline", response = NULL,
                      alpha.prior = NULL, beta.prior = NULL, gamma.prior = NULL, dc.prior = NULL, c1.prior = NULL,
                      rho.prior = NULL, hy.prior = NULL){

  nobs <- length(Y)

  Treat.name <- unique(Treat)
  ntreat <- length(Treat.name)
  if(!is.null(baseline)){
    if(!baseline %in% Treat){
      stop("wrong baseline name")
    }
    Treat.name <- Treat.name[Treat.name != baseline]
  } else{
    Treat.name <- Treat.name[-1]
  }
  nof1 = list(Y = Y, Treat = Treat, baseline = baseline, ncat = ncat, nobs = nobs, Treat.name = Treat.name, ntreat = ntreat, response = response)

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

  if(response == "ordinal"){
    if(is.null(ncat)){
      stop("ncat (number of categories) must be entered for ordinal response")
    }
  }

  prior.param <- list(response = response, dc.prior = dc.prior, c1.prior = c1.prior, alpha.prior = alpha.prior, beta.prior = beta.prior, gamma.prior = gamma.prior, hy.prior = hy.prior, rho.prior = rho.prior)
  prior.data <- nof1.prior.default(prior.param)

  nof1 <- c(nof1, prior.data)

  code <- nof1.rjags(nof1)
  nof1$code <- code

  class(nof1) <- "nof1.data"
  nof1
}



