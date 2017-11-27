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

#' Stacked_percent_barplot for raw data (for ordinal data)
#'
#' @param nof1 nof1 object created using nof1.data
#' @export

stacked_percent_barplot <- function(nof1){
  
  if(nof1$response %in% c("binomial", "ordinal")){
    data <- aggregate(nof1$Y, list(Y = nof1$Y, Treat = nof1$Treat), length)
    #ggplot(data, aes(fill= Treat, y= x, x= Y)) + geom_bar( stat="identity", position="fill") + ylab("proportion")
    ggplot(data, aes(fill= factor(Y), y= x, x= Treat)) + geom_bar( stat="identity", position="fill") + labs(fill = "Outcomes") + ylab("proportions")
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
  ggplot(data = data, aes(1:length(Y), Y, color = factor(Treat))) + geom_point() + xlab("Time") + labs(color = "Treatment")
}


