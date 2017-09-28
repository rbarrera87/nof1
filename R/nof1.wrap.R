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


check_enough_data <- function(Treatment, x){
  length(table(Treatment[!is.na(x)])) == 3
}

comparison <- function(x, response){
  answer <-
    if(response == "poisson" || response == "binomial"){
      exp(x)
    } else if(response == "normal"){
      x
    }
  return(answer)
}

find_raw_mean <- function(Y, Treat, baseline, response){

  c(mean(Y[Treat == baseline], na.rm = TRUE), mean(Y[Treat == "A"], na.rm = TRUE), mean(Y[Treat == "B"], na.rm = TRUE))
}

round_raw_mean <- function(raw_mean, response){

  if(response == "poisson" || response == "normal"){
    round(raw_mean,1)
  } else if(response == "binomial"){
    round(raw_mean*100)
  }
}

find_mean_difference <- function(coef, response, raw_mean){

  rounded <-
  if(response == "poisson"){

    round(
    c(base_vs_scd = (exp(coef["beta_A"]) - 1) * raw_mean[1], base_vs_mscd = (exp(coef["beta_B"]) - 1) * raw_mean[1],
     mscd_vs_scd = (exp(coef["beta_A"] - coef["beta_B"]) - 1) * raw_mean[3]), 1)

  } else if (response == "binomial"){

    # can't divide by 0
    if(raw_mean[1] == 1) raw_mean[1] = 0.99
    if(raw_mean[3] == 1) raw_mean[3] = 0.99

    round(
    c(base_vs_scd = (raw_mean[1]/(1-raw_mean[1]) * exp(coef["beta_A"])) / (1 + raw_mean[1]/(1-raw_mean[1]) * exp(coef["beta_A"])) - raw_mean[1],
      base_vs_mscd = (raw_mean[1]/(1-raw_mean[1]) * exp(coef["beta_B"])) / (1 + raw_mean[1]/(1-raw_mean[1]) * exp(coef["beta_B"])) - raw_mean[1],
      mscd_vs_scd = (raw_mean[3]/(1-raw_mean[3]) * exp(coef["beta_A"] - coef["beta_B"])) / (1 + raw_mean[3]/(1-raw_mean[3]) * exp(coef["beta_A"] - coef["beta_B"])) - raw_mean[3]
      ) * 100)
  } else if (response == "normal"){

    round(
    c(base_vs_scd = coef["beta_A"],
      base_vs_mscd = coef["beta_B"],
      mscd_Vs_scd = coef["beta_A"] - coef["beta_B"]), 1)
  }

  if(response == "binomial"){
    rounded[rounded==0] <- 1
    rounded[rounded==100] <- 99
  }
  return(rounded)
}

calculate_p_threshold <- function(samples, response){

  upper <-
    if(response == "poisson"){
      1.1
    } else if(response == "binomial"){
      1.1
    } else if(response == "normal"){
      2.9
    }

  lower <-
    if(response == "poisson"){
      0.9
    } else if(response == "binomial"){
      0.9
    } else if(response == "normal"){
      -2.9
    }

  base_vs_scd <- list(greater_than_threshold = round(mean(comparison(samples[,"beta_A"], response) > upper, na.rm = TRUE)*100),
                      lower_than_threshold = round(mean(comparison(samples[,"beta_A"], response) < lower, na.rm = TRUE)*100))

  base_vs_mscd <- list(greater_than_threshold = round(mean(comparison(samples[,"beta_B"], response) > upper, na.rm = TRUE)*100),
                      lower_than_threshold = round(mean(comparison(samples[,"beta_B"], response) < lower, na.rm = TRUE)*100))

  mscd_vs_scd <- list(greater_than_threshold = round(mean(comparison(samples[,"beta_A"] - samples[,"beta_B"], response) > upper, na.rm = TRUE)*100),
                        lower_than_threshold = round(mean(comparison(samples[,"beta_A"] - samples[,"beta_B"], response) < lower, na.rm = TRUE)*100))

  change <- function(x){
    x = ifelse(x==0,1,x)
    x = ifelse(x==100,99,x)
    return(x)
  }
  base_vs_scd <- rapply(base_vs_scd, change, how = "replace")
  base_vs_mscd <- rapply(base_vs_mscd, change, how = "replace")
  mscd_vs_scd <- rapply(mscd_vs_scd, change, how = "replace")

  return(list(base_vs_scd = base_vs_scd, base_vs_mscd = base_vs_mscd, mscd_vs_scd = mscd_vs_scd))
}

#' Summarizes the result from the model into json format
#'
#' @export

summarize_nof1 <- function(nof1, result){

  with(c(nof1, result),{

    samples <- do.call(rbind, samples)
    raw_mean <- find_raw_mean(Y, Treat, baseline, response)
    rounded_raw_mean <- round_raw_mean(raw_mean, response)

    coef <- apply(samples[,c("beta_A", "beta_B")], 2, mean)
    diff <- find_mean_difference(coef, response, raw_mean)

    raw_mean <- list(base = rounded_raw_mean[1], scd = rounded_raw_mean[2], mscd = rounded_raw_mean[3])
    mean_difference <- list(base_vs_scd = diff[1], base_vs_mscd = diff[2], mscd_vs_scd = diff[3])
    three_group_comparison <- list(raw_mean = raw_mean, mean_difference = mean_difference)

    gauge_graph <- calculate_p_threshold(samples, response)
    return(list(three_group_comparison = three_group_comparison, gauge_graph = gauge_graph))

  })
}

# washout <- function(read_data){
#
#   with(read_data,{
#     delete_obs <- rle(Treatment)
#     rle(Treatment)
#
#   })
#
#
# }

#' Wrapper function that runs the n-of-1 model
#'
#' @param json.file input json data
#' @export

wrap <- function(data, metadata){

  read_data <- tryCatch({
    read_input_data(data, metadata)
  }, error = function(error){
    return(paste("input read error: ", error))
  })

  # washout(read_data)


  stool_frequency <- tryCatch({
    data <- list(Treat = read_data$Treatment, Y = read_data$stool_frequency)
    nof1 <- with(data, {
      nof1.data(Y, Treat, response = "poisson")
    })
    result <- nof1.run(nof1)
    summarize_nof1(nof1, result)
  }, error = function(error){
    return(paste("stool_frequency run error: ", error))
  })

  stool_consistency <- tryCatch({
    data <- list(Treat = read_data$Treatment, Y = read_data$stool_consistency)
    nof1 <- with(data, {
      nof1.data(Y, Treat, response = "binomial")
    })
    result <- nof1.run(nof1)
    summarize_nof1(nof1, result)
  }, error = function(error){
    return(paste("stool_consistency run error: ", error))
  })

  pain_interference <- tryCatch({
    data <- list(Treat = read_data$Treatment_weekly, Y = read_data$pain_interference)
    nof1 <- with(data, {
      nof1.data(Y, Treat, response = "normal")
    })
    result <- nof1.run(nof1)
    summarize_nof1(nof1, result)
  }, error = function(error){
    return(paste("pain_interference run error: ", error))
  })

  gi_symptoms <- tryCatch({
    data <- list(Treat = read_data$Treatment_weekly, Y = read_data$gi_symptoms)
    nof1 <- with(data, {
      nof1.data(Y, Treat, response = "normal")
    })
    result <- nof1.run(nof1)
    summarize_nof1(nof1, result)
  }, error = function(error){
    return(paste("gi_symptoms run error: ", error))
  })

  check_success <- function(x){
    ifelse(is.list(x), TRUE, x)
  }

  metadata <- list(successful_input_reading = check_success(read_data),
                   successful_run_stool_frequency = check_success(stool_frequency),
                   successful_run_stool_consistency = check_success(stool_consistency),
                   successful_run_pain_interference = check_success(pain_interference),
                   successful_run_gi_symptoms = check_success(gi_symptoms),
                   enough_stool_consistency = check_enough_data(read_data$Treatment, read_data$stool_consistency),
                   enough_stool_frequency = check_enough_data(read_data$Treatment, read_data$stool_frequency),
                   enough_pain_interference = check_enough_data(read_data$Treatment_weekly, read_data$pain_interference),
                   enough_gi_symptoms = check_enough_data(read_data$Treatment_weekly, read_data$gi_symptoms),
                   user_id = 325,
                   timestamp_trialist_completed = Sys.time(),
                   trialist_version_id = 1,
                   trialist_version_date = "8/15/2017",
                   trialist_version_note = "")

  summary_graph <- tryCatch({
    base_vs_scd <- find_summary_graph(metadata, "base_vs_scd", stool_frequency, stool_consistency, pain_interference, gi_symptoms)
    base_vs_mscd <- find_summary_graph(metadata, "base_vs_mscd", stool_frequency, stool_consistency, pain_interference, gi_symptoms)
    mscd_vs_scd <- find_summary_graph(metadata, "mscd_vs_scd", stool_frequency, stool_consistency, pain_interference, gi_symptoms)
    list(base_vs_scd = base_vs_scd, base_vs_mscd = base_vs_mscd, mscd_vs_scd = mscd_vs_scd)
  }, error = function(error){
    return(paste("error in summary step:", error))
  })

  metadata2 <- list(successful_summary_graph = check_success(summary_graph))
  metadata <- c(metadata2, metadata)

  final <- list(metadata = metadata, stool_frequency = stool_frequency, stool_consistency = stool_consistency,
                pain_interference = pain_interference, gi_symptoms = gi_symptoms, summary_graph = summary_graph)

  return(final)
}

find_summary_graph <- function(metadata, treatment_comparison,stool_frequency, stool_consistency, pain_interference, gi_symptoms){

  summary <- list()

  if(metadata$successful_run_stool_frequency == TRUE){
    a <- stool_frequency[["gauge_graph"]][[treatment_comparison]][["greater_than_threshold"]]
    b <- stool_frequency[["gauge_graph"]][[treatment_comparison]][["lower_than_threshold"]]
    summary$stool_frequency <- round(ifelse(a >= b, -a, b))
  }

  if(metadata$successful_run_stool_consistency == TRUE){
    c <- stool_consistency[["gauge_graph"]][[treatment_comparison]][["greater_than_threshold"]]
    d <- stool_consistency[["gauge_graph"]][[treatment_comparison]][["lower_than_threshold"]]
    summary$stool_consistency <- round(ifelse(c >= d, -c, d))
  }

  if(metadata$successful_run_pain_interference == TRUE){
    e <- pain_interference[["gauge_graph"]][[treatment_comparison]][["greater_than_threshold"]]
    f <- pain_interference[["gauge_graph"]][[treatment_comparison]][["lower_than_threshold"]]
    summary$pain_interference <- round(ifelse(e >= f, -e, f))
  }

  if(metadata$successful_run_gi_symptoms == TRUE){
    g <- gi_symptoms[["gauge_graph"]][[treatment_comparison]][["greater_than_threshold"]]
    h <- gi_symptoms[["gauge_graph"]][[treatment_comparison]][["lower_than_threshold"]]
    summary$gi_symptoms <- round(ifelse(g >= h, -g, h))
  }
  return(summary)
}

