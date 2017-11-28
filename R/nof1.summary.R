#' Frequency plot for raw data
#'
#' @param nof1 nof1 object created using nof1.data
#' @export

frequency_plot <- function(nof1){
  
  # a <- nof1.normal.simulation()
  # a <- nof1.poisson.simulation()
  # a <- nof1.ordinal.simulation()
  # nof1 <- nof1.data(a$Y, a$Treat, ncat = 5, response = "ordinal")
  
  if(nof1$response %in% c("binomial", "ordinal")){
    data <- aggregate(nof1$Y, list(Y = nof1$Y, Treat = nof1$Treat), length)
    ggplot(data= data, aes(x= Y, y= x, fill=Treat)) +  geom_bar(stat="identity", position="dodge") + ylab("Frequency") + xlim(0.5, nof1$ncat)+ theme_bw()  
  } else if(nof1$response %in% c("normal", "poisson")){
    data <- data.frame(Y = nof1$Y, Treat = nof1$Treat)
    ggplot(data, aes(x = Y, fill = Treat, color = Treat)) + geom_histogram(position = "dodge", alpha = 0.5) + theme_bw()
  }
}

#' Stacked_percent_barplot for raw data (for ordinal or binomial data)
#'
#' @param nof1 nof1 object created using nof1.data
#' @export

stacked_percent_barplot <- function(nof1){
  
  if(nof1$response %in% c("binomial", "ordinal")){
    data <- aggregate(nof1$Y, list(Y = nof1$Y, Treat = nof1$Treat), length)
    #ggplot(data, aes(fill= Treat, y= x, x= Y)) + geom_bar( stat="identity", position="fill") + ylab("Percentage")
    ggplot(data, aes(fill= factor(Y), y= x, x= Treat)) + geom_bar( stat="identity", position="fill") + scale_y_continuous(labels = percent_format()) + labs(fill = "Outcomes") + ylab("Percentages") +  theme_bw()  
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
#' @param nof1 nof1 object created using nof1.data
#' @export

time_series_plot <- function(nof1){
  
  data <- data.frame(Y = nof1$Y, Treat = nof1$Treat)
  ggplot(data = data, aes(1:length(Y), Y, color = factor(Treat), group = 1)) + geom_point() + geom_line()+ labs(x = "Time", y = "Outcomes", color = "Treatment") + scale_y_continuous(breaks=1:nof1$ncat) +  ylim(1, nof1$ncat) +  theme_bw()  
}



#' Kernel density of the posterior distribution for odds ratio
#'
#' @param result nof1 result object created using nof1.run
#' @export

kernel_plot <- function(result){
  samples <- do.call(rbind, result$samples)
  beta_variable <- samples[,grep("beta", colnames(samples))]
  data <- as.data.frame(beta_variable)
  
  ggplot(data, aes(beta_variable)) + geom_density() + theme_bw() + labs(x = "log odds ratio")
}




#' Odds ratio plot for the raw data
#'
#' @param result.list list of nof1 results created using nof1.run
#' @param name of the outcomes. If left unspecified, it numbers each result in order of how it is stored in result.list
#' @param level confidence interval level (default is 0.95)
#' @export

odds_ratio_plot <- function(result.list, result.name = NULL, level = 0.95){
  
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
    labs(x = "Variables", y = "OR") +
    theme_bw()  
}

#' Odds ratio plot for the raw data
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


#' Plot for probability that odds ratio is greater than 1
#'
#' @param result.list list of nof1 results created using nof1.run
#' @param result.name name of the outcomes
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
  
  ggplot(data, aes(fill = factor(Treat), y = probability, x = result.name)) + geom_bar( stat="identity", position="fill") + scale_y_continuous(labels = percent_format()) + labs(title = "Percentage certain treatment is better", x = "Variables", y = "Percentages", fill = "Treatment") + coord_flip()  +  theme_bw()  
}




# 
# plot_odds<-function(x, title = NULL){
#   tmp<-data.frame(cbind(exp(x), exp(confint(x))))
#   odds<-tmp[-1,]
#   names(odds)<-c("OR", "lower", "upper")
#   odds$vars<-row.names(odds)
#   ticks<-c(seq(.1, 1, by =.1), seq(0, 10, by =1), seq(10, 100, by =10))
#   
#   ggplot(odds, aes(y= OR, x = reorder(vars, OR))) +
#     geom_point() +
#     geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
#     scale_y_log10(breaks=ticks, labels = ticks) +
#     geom_hline(yintercept = 1, linetype=2) +
#     coord_flip() +
#     labs(title = title, x = "Variables", y = "OR") +
#     theme_bw()
# }

