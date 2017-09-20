read_input_data <- function(json.file){
  
  data <- fromJSON(json.file, flatten = TRUE)
  
  if(data$metadata$user_age < 14){
    selection_names <- c("treatment", "parent_response.daily_stool_consistency", "parent_response.daily_stool_frequency",
                         "parent_response.promis_pain_interference", "parent_response.promis_gi_symptoms")
  }else{
    
    null_count <- apply(data$data, 2, function(x){ sum(is.na(unlist(x)))})
    
  }
  
  #under 14, no child input
  
  
  selection_names <- c("treatment", "parent_response.daily_stool_consistency", "parent_response.daily_stool_frequency",
                     "parent_response.promis_pain_interference", "parent_response.promis_gi_symptoms")
  
  dataset <- data$data[,selection_names]
  
  Treatment <- dataset[,"treatment"]
  Treatment <- unlist(Treatment)
  Treatment[Treatment == "strict"] = "A"
  Treatment[Treatment == "liberalized"] = "B"
  Treatment_weekly <- Treatment[seq(1, length(Treatment), 7)]
  
  Outcome <- dataset[, !(names(dataset) %in% "treatment")]
  Outcome <- apply(Outcome, 2, unlist)
  Outcome[["parent_response.daily_stool_consistency"]] <- ifelse(1 < Outcome[["parent_response.daily_stool_consistency"]] & Outcome[["parent_response.daily_stool_consistency"]] < 7, 0, 1)
  
  list(Treatment = Treatment, Treatment_weekly = Treatment_weekly, stool_consistency = Outcome$parent_response.daily_stool_consistency, stool_frequency = Outcome$parent_response.daily_stool_frequency, pain_interference =  Outcome$parent_response.promis_pain_interference,  gi_symptoms = Outcome$parent_response.promis_gi_symptoms)
}


#############

nof1.data <- function(Y, Treat, Time=NULL, ncat = NULL, knots = NULL, baseline = NULL, response = NULL, 
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



