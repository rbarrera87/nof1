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
    ggplot(data= data, aes(x= Y, y= x, fill=Treat)) +  geom_bar(stat="identity", position="dodge") + ylab("Frequency")  
  } else if(nof1$response %in% c("normal", "poisson")){
    data <- data.frame(Y = nof1$Y, Treat = nof1$Treat)
    ggplot(data, aes(x = Y, fill = Treat, color = Treat))  + geom_histogram(position = "dodge", alpha = 0.5)
  }
}

#' Stacked_percent_barplot for raw data (for ordinal or binomial data)
#'
#' @param nof1 nof1 object created using nof1.data
#' @export

stacked_percent_barplot <- function(nof1){
  
  if(nof1$response %in% c("binomial", "ordinal")){
    data <- aggregate(nof1$Y, list(Y = nof1$Y, Treat = nof1$Treat), length)
    #ggplot(data, aes(fill= Treat, y= x, x= Y)) + geom_bar( stat="identity", position="fill") + ylab("proportion")
    ggplot(data, aes(fill= factor(Y), y= x, x= Treat)) + geom_bar( stat="identity", position="fill") + labs(fill = "Outcomes") + ylab("Proportions")
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
  ggplot(data = data, aes(1:length(Y), Y, color = factor(Treat))) + geom_point() +ylab("Outcomes") + xlab("Time") + labs(color = "Treatment") + scale_y_continuous(breaks=1:nof1$ncat)
}


#' Odds ratio plot for the raw data
#'
#' @param result.list list of nof1 results created using nof1.run
#' @param result.name name of the outcome 
#' @export

odds_ratio_plot <- function(result.list, result.name = NULL){
  
  odds_ratio <- matrix(NA, nrow = length(result.list), ncol = 3)
  
  for(i in 1:length(result.list)){
    result <- result.list[[i]]
    samples <- do.call(rbind, result$samples)
    odds_ratio[i,] <- exp(quantile(samples[,grep("beta", colnames(samples))], c(0.025, 0.5, 0.975)))
  }
  
  odds <- as.data.frame(odds_ratio)
  names(odds) <- c("lower", "OR", "upper")
  
  if(is.null(result.name)){
    odds$vars <- row.names(odds)  
  } else{
    odds$vars <- result.name
  }
  ticks <- c(0.1, 0.2, 0.5, 1, 2, 5, 10)
  ggplot(odds, aes(y = OR, x = reorder(vars, OR))) + 
    geom_point() +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = .2) +
    scale_y_log10(breaks = ticks, labels = ticks) +
    geom_hline(yintercept = 1, linetype = 2) +
    coord_flip() +
    labs(x = "Variables", y = "OR") +
    theme_bw()  
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
# 
# plot_odds(result$samples)
