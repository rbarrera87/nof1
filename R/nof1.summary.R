#' time series plot across different interventions
#' 
#' @param nof1 nof1 object created using nof1.data
#' @export

time_series_plot2 <- function(nof1, time = NULL, timestamp = NULL, timestamp.format = "%m/%d/%Y %H:%M", Outcome.name = ""){
  
  # if(!is.null(time)){
  #   time_difference <- time
  # } else if(is.null(timestamp)){
  #   time_difference <- 1:length(nof1$Y)
  # } else if(!is.null(timestamp)){
  #   first_timestamp <- strptime(timestamp[1], timestamp.format)
  #   
  #   time_difference <- rep(NA, length(timestamp))
  #   time_difference[1] <- 1 
  #   for(i in 2:length(timestamp)){
  #     second_timestamp <- strptime(timestamp[i], timestamp.format)
  #     time_difference[i] <- round(1 + as.numeric(difftime(second_timestamp, first_timestamp, units = "days")))
  #   }  
  # }
  
  date <- as.Date(timestamp, timestamp.format)
  
  data <- data.frame(Y = as.numeric(nof1$Y), Treatment = gsub("\\_", " ", nof1$Treat), date = date)
  data2 <- aggregate(nof1$Y, list(Treatment = gsub("\\_", " ", nof1$Treat)), mean)
    
  ggplot(data, aes(x=date, Y, fill = Treatment)) + geom_bar(stat = "identity")  + facet_grid(. ~ Treatment) + theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
    labs(x = "Date", y = Outcome.name) +
    geom_hline(data = data2, aes(yintercept = x, linetype = "Mean"), color="black") + theme(plot.title = element_text(hjust = 0.5)) + 
    scale_y_continuous(breaks = 0:nof1$ncat, oob = rescale_none, label = c("Low", rep("", length = nof1$ncat -1), "High")) +
    scale_fill_manual(values=c("#adc2eb", "#ffb380")) +
    scale_linetype_manual(name = "", values = 1, guide = guide_legend(override.aes = list(color = c("black"))))
    
}



#' Frequency plot for raw data
#'
#' @param nof1 nof1 object created using nof1.data
#' @export

frequency_plot <- function(nof1, xlab = NULL, title = NULL){
  
  if(nof1$response %in% c("binomial", "ordinal")){
    data <- aggregate(nof1$Y, list(Y = nof1$Y, Treat = nof1$Treat), length)
    ggplot(data= data, aes(x= Y, y= x, fill=Treat)) +  geom_bar(stat="identity", position="dodge") + labs(title = title, x = xlab, y = "Frequency", fill = "Outcomes") + xlim(0.5, nof1$ncat +0.5)+ theme_bw()  
  } else if(nof1$response %in% c("normal", "poisson")){
    data <- data.frame(Y = nof1$Y, Treat = nof1$Treat)
    ggplot(data, aes(x = Y, fill = Treat, color = Treat)) + geom_histogram(position = "dodge", alpha = 0.5) + labs(title = title, x = xlab) + theme_bw()
  }
}

#' Stacked_percent_barplot for raw data (for ordinal or binomial data)
#'
#' @param nof1 nof1 object created using nof1.data
#' @export

stacked_percent_barplot <- function(nof1, title = NULL){
  
  if(nof1$response %in% c("binomial", "ordinal")){
    data <- aggregate(nof1$Y, list(Y = nof1$Y, Treat = nof1$Treat), length)
    
    ggplot(data, aes(fill= factor(Y, levels = 1:nof1$ncat), y= x, x= Treat)) + geom_bar( stat="identity", position="fill") + 
      scale_y_continuous(labels = percent_format()) +  theme_bw() + labs(title = title, x = "Treatment", y = "Percentage", fill = "Outcomes") +
      scale_fill_manual(values = 4:(3+nof1$ncat), labels = 1:nof1$ncat, drop = FALSE)
  } else{
    stop("only works for binomial and ordinal data")
  }
}

#' Summary data table for nof1
#'
#' @param nof1 nof1 object created using nof1.data
#' @export

raw_table <- function(nof1){
  
  if(nof1$response %in% c("binomial", "ordinal")){
    table(nof1$Y, nof1$Treat)
  } else if(nof1$response %in% c("normal", "poisson")){
    raw_table <- aggregate(nof1$Y, list(Treat = nof1$Treat), mean)
    colnames(raw_table)[2] <- "mean"
    cbind(raw_table, sd = aggregate(nof1$Y, list(Treat = nof1$Treat), sd)[,-1], aggregate(nof1$Y, list(Treat = nof1$Treat), quantile, c(0.025, 0.5, 0.975))[,-1])
  }
}


#' Time series plot for the raw data
#'
#' Draw time series plot
#'
#' @param nof1 nof1 object created using nof1.data
#' @param time can manually specify time variable
#' @param timestamp or instead provide timestamp information for the all the outcomes
#' @param timestamp.format format of timestamp used. See default format.
#' @export

time_series_plot <- function(nof1, time = NULL, timestamp = NULL, timestamp.format = "%m/%d/%Y %H:%M"){
  
  if(!is.null(time)){
    time_difference <- time
  } else if(is.null(timestamp)){
    time_difference <- 1:length(nof1$Y)
  } else if(!is.null(timestamp)){
    first_timestamp <- strptime(timestamp[1], timestamp.format)
    
    time_difference <- rep(NA, length(timestamp))
    time_difference[1] <- 1 
    for(i in 2:length(timestamp)){
      second_timestamp <- strptime(timestamp[i], timestamp.format)
      time_difference[i] <- round(1 + as.numeric(difftime(second_timestamp, first_timestamp, units = "days")))
    }  
  } 
  
  data <- data.frame(Y = nof1$Y, Treat = nof1$Treat)
  ggplot(data = data, aes(time_difference, Y, color = factor(Treat), group = 1)) + geom_point() + labs(x = "Time", y = "Outcomes", color = "Treatment") +  ylim(1, nof1$ncat) +  theme_bw()  
}



#' Kernel density of the posterior distribution for odds ratio
#'
#' @param result nof1 result object created using nof1.run
#' @export

kernel_plot <- function(result, xlim_value = c(0, 10), title = NULL){
  samples <- do.call(rbind, result$samples)
  beta_variable <- exp(samples[,grep("beta", colnames(samples))])
  data <- as.data.frame(beta_variable)
  
  ggplot(data, aes(beta_variable)) + geom_density() + theme_bw() + xlim(xlim_value[1], xlim_value[2]) + labs(title = title, x = "Odds Ratio", y = "Density") 
}


#' Odds ratio plot for the raw data
#'
#' @param result.list list of nof1 results created using nof1.run
#' @param name of the outcomes. If left unspecified, it numbers each result in order of how it is stored in result.list
#' @param level confidence interval level (default is 0.95)
#' @export

odds_ratio_plot <- function(result.list, result.name = NULL, level = 0.95, title = NULL){
  
  odds_ratio <- matrix(NA, nrow = length(result.list), ncol = 3)
  
  for(i in 1:length(result.list)){
    result <- result.list[[i]]
    samples <- do.call(rbind, result$samples)
    odds_ratio[i,] <- exp(quantile(samples[,grep("beta", colnames(samples))], c((1 -level)/2, 0.5, 1 - (1 -level)/2)))
  }
  
  odds <- as.data.frame(odds_ratio)
  names(odds) <- c("lower", "OR", "upper")
  
  if(is.null(result.name)){
    odds$vars <- row.names(odds)  
  } else{
    if(length(result.name) != length(result.list)){
      stop("result.name should have same length as result.list")
    }
    odds$vars <- result.name
  }
  ticks <- c(0.1, 0.2, 0.5, 1, 2, 5, 10)
  ggplot(odds, aes(y = OR, x = factor(vars))) + 
    geom_point() +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = .2) +
    scale_y_log10(breaks = ticks, labels = ticks) +
    geom_hline(yintercept = 1, linetype = 2) +
    coord_flip() +
    labs(x = "Variables", y = "Odds Ratio", title = title) +
    theme_bw()  
}

#' Plot showing probability certain treatment is better than the other one
#'
#' @param result.list list of nof1 results created using nof1.run
#' @param result.name name of the outcomes. If left unspecified, it numbers each result in order of how it is stored in result.list
#' @export

probability_barplot <- function(result.list, result.name = NULL){
  
  probability <- rep(NA, length(result.list)* 2)
  
  for(i in 1:length(result.list)){
    result <- result.list[[i]]
    samples <- do.call(rbind, result$samples)
    probability[(i-1)*2 + 1] <- mean(exp(samples[,grep("beta", colnames(samples))]) > 1)
    probability[i*2] <- 1 - probability[(i-1)*2 + 1] 
  }
  
  if(is.null(result.name)){
    result.name <- rep(1:length(result.list), each = 2)
  } else{
    if(length(result.name) != length(result.list)){
      stop("result.name should have same length as result.list")
    }
    result.name <- rep(result.name, each = 2)
  }
  
  data <- data.frame(probability = probability, result.name = result.name, Treat = rep(c(levels(result.list$result$nof1$Treat)[2],levels(result.list$result$nof1$Treat)[1]), length(result.list)))
  
  ggplot(data, aes(fill = factor(Treat), y = probability, x = result.name)) + geom_bar( stat="identity", position="fill") + scale_y_continuous(labels = percent_format()) + labs(x = "Variables", y = "Percentages", fill = "Treatment") + coord_flip()  +  theme_bw()  
}