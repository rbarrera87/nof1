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

summarize_nof1 <- function(nof1, result, outcome){

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

#' Make a network object containing data, priors, and a jags model file
#'
#' @param json.file input json data
#' @export

wrap <- function(json.file){

  read_data <- tryCatch({
    read_input_data(json.file)
  }, error = function(error){
    return(paste("input read error: ", error))
  })

  stool_frequency <- tryCatch({
    data <- list(Treat = read_data$Treatment, Y = read_data$stool_frequency)
    nof1 <- with(data, {
      nof1.data(Y, Treat, baseline = "baseline", response = "poisson")
    })
    result <- nof1.run(nof1)
    summarize_nof1(nof1, result, "stool_frequency")
  }, error = function(error){
    return(paste("stool_frequency run error: ", error))
  })

  stool_consistency <- tryCatch({
    data <- list(Treat = read_data$Treatment, Y = read_data$stool_consistency)
    nof1 <- with(data, {
      nof1.data(Y, Treat, baseline = "baseline", response = "binomial")
    })
    result <- nof1.run(nof1)
    summarize_nof1(nof1, result, "stool_consistency")
  }, error = function(error){
    return(paste("stool_consistency run error: ", error))
  })

  pain_interference <- tryCatch({
    data <- list(Treat = read_data$Treatment_weekly, Y = read_data$pain_interference)
    nof1 <- with(data, {
      nof1.data(Y, Treat, baseline = "baseline", response = "normal")
    })
    result <- nof1.run(nof1)
    summarize_nof1(nof1, result, "pain_interference")
  }, error = function(error){
    return(paste("pain_interference run error: ", error))
  })

  gi_symptoms <- tryCatch({
    data <- list(Treat = read_data$Treatment_weekly, Y = read_data$gi_symptoms)
    nof1 <- with(data, {
      nof1.data(Y, Treat, baseline = "baseline", response = "normal")
    })
    result <- nof1.run(nof1)
    summarize_nof1(nof1, result, "gi_symptoms")
  }, error = function(error){
    return(paste("gi_symptoms run error: ", error))
  })

  check_success <- function(x){
    ifelse(is.list(x), TRUE, x)
  }

  tryCatch({
  metadata <- list(successful_input_reading = check_success(read_data),
                   successful_run_stool_frequency = check_success(stool_frequency),
                   successful_run_stool_consistency = check_success(stool_consistency),
                   successful_run_pain_interference = check_success(pain_interference),
                   successful_run_gi_symptoms = check_success(gi_symptoms),
                   user_id = 325,
                   timestamp_trialist_completed = Sys.time(),
                   trialist_version_id = 1,
                   trialist_version_date = "8/15/2017",
                   trialist_version_note = "")

  base_vs_scd <- find_summary_graph(metadata, "base_vs_scd", stool_frequency, stool_consistency, pain_interference, gi_symptoms)
  base_vs_mscd <- find_summary_graph(metadata, "base_vs_mscd", stool_frequency, stool_consistency, pain_interference, gi_symptoms)
  mscd_vs_scd <- find_summary_graph(metadata, "mscd_vs_scd", stool_frequency, stool_consistency, pain_interference, gi_symptoms)
  }, error = function(error){
    return(paste("error in summary step:", error))
  })

  summary_graph <- list(base_vs_scd = base_vs_scd, base_vs_mscd = base_vs_mscd, mscd_vs_scd = mscd_vs_scd)

  final <- list(metadata = metadata, stool_frequency = stool_frequency, stool_consistency = stool_consistency,
                pain_interference = pain_interference, gi_symptoms = gi_symptoms, summary_graph = summary_graph)

  output <- toJSON(final, pretty = TRUE, UTC = TRUE, auto_unbox = TRUE)
  return(output)
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

