#' Run nof1 model
#'
#' @param nof1 nof1 object created using nof1.data
#' @export

nof1.run <- function(nof1, inits = NULL, n.chains = 3, max.run = 100000, setsize = 10000, n.run = 50000,
                     conv.limit = 1.05, extra.pars.save = NULL){

  if (!inherits(nof1, "nof1.data")) {
    stop('Given network is not nof1.data. Run nof1.data function first')
  }

  if(max.run < setsize){
    stop("setsize should be smaller than max.run")
  }

  with(nof1, {

  pars.save <- ifelse(response == "ordinal", "c", "alpha")

  if(response == "binomial"){
    
    pars.save <- c(pars.save, paste0("p_", baseline))
    for(i in Treat.name){
      pars.save <- c(pars.save, paste0("p_", i))  
    }
    
    comps <- combn(Treat.order, 2)
    for(i in 1:ncol(comps)){
      pars.save <- c(pars.save, paste0("RR_", comps[1,i], "_", comps[2,i]))
    }
  }

  if(response == "normal"){
    pars.save <- c(pars.save, "logprec")
  }

  # if(!is.null(knots)){
  #   for(i in 1:ncol(BS)){
  #     pars.save <- c(pars.save, paste0("gamma", i))
  #   }
  # }

  for(i in Treat.name){
    pars.save <- c(pars.save, paste0("beta_", i))
  }

  data <- list(Y = Y)
  for(i in Treat.name){
    data[[paste0("Treat_", i)]] <- nof1[[paste0("Treat_", i)]]
  }

  # if(!is.null(knots)){
  #   data$BS <- nof1$BS
  # }

  if(is.null(inits)){
    inits <- nof1.inits(nof1, n.chains)
  }
  samples <- jags.fit(nof1, data, pars.save, inits, n.chains, max.run, setsize, n.run, conv.limit)

  result <- list(nof1 = nof1, inits = inits, pars.save = pars.save, data.rjags = data)
  result <- c(result, samples)

  class(result) <- "nof1.result"
  return(result)
  })
}

jags.fit <- function(nof1, data, pars.save, inits, n.chains, max.run, setsize, n.run, conv.limit){

  mod = rjags::jags.model(textConnection(nof1$code), data = data, inits = inits, n.chains = n.chains, n.adapt = setsize)
  
  adapted <- FALSE
  count <- 0
  while(!adapted){
    adapted <- rjags::adapt(mod, setsize, end.adaptation = FALSE)
    count <- count + 1
    if(count == 100){
      stop("algorithm has not adapted")
    }
  }
  
  samples <- rjags::coda.samples(model = mod, variable.names = pars.save, n.iter = setsize)

  max.gelman <- find.max.gelman(samples)
  print(max.gelman)
  check <- max.gelman > conv.limit

  if(check) {
    count <- 1
    while (check & count < max.run/setsize) {
      samples2 <- rjags::coda.samples(mod, variable.names = pars.save, n.iter = setsize)
      samples <- add.mcmc(samples, samples2)

      count <- count + 1

      max.gelman <- find.max.gelman(samples)
      check <- max.gelman > conv.limit
      print(max.gelman)
    }
  }

  start <- mcpar(samples[[1]])[1]
  end <- mcpar(samples[[1]])[2]
  mid <- (end + start-1)/2
  burnin <- ceiling(end - mid)
  samples <- window(samples, mid+1, end, 1) #keep the last half of the converged sequence
  samples <- new.mcmc(samples)

  n.thin <- 1
  if(check == TRUE){
    stop("code didn't converge according to gelman-rubin diagnostics")
  } else if(n.run < burnin){
    n.thin <- ceiling(burnin/n.run)
    extra.run <- n.run * n.thin - burnin
    if(extra.run != 0){
      samples2 <- rjags::coda.samples(mod, variable.names = pars.save, n.iter = extra.run)
      samples <- add.mcmc(samples, samples2)
    }
    samples <- window(samples, 1, dim(samples[[1]])[1], n.thin)
  } else if(n.run > burnin){
    extra.run <- n.run - burnin
    samples2 <- rjags::coda.samples(mod, variable.names = pars.save, n.iter = extra.run)
    samples <- add.mcmc(samples, samples2)
  }
  max.gelman <- find.max.gelman(samples)
  print(max.gelman)

  out <-list(burnin = burnin, n.thin = n.thin, samples = samples, max.gelman = max.gelman)
  return(out)
}


new.mcmc <- function(x){
  n.chains <- length(x)
  n.var <- coda::nvar(x)
  newobjects <- vector("list", length = n.chains)

  for(i in 1:n.chains){
    newobjects[[i]] <- matrix(NA, nrow = 0, ncol = n.var, dimnames = list(NULL, dimnames(x[[1]])[[2]]))
    newobjects[[i]] <- x[[i]]
    newobjects[[i]] <- mcmc(newobjects[[i]])
  }
  mcmc.list(newobjects)
}

add.mcmc <- function(x, y){

  n.chains <- length(x)
  n.var <- coda::nvar(x)
  newobjects <- vector("list", length = n.chains)

  for(i in 1:n.chains){
    newobjects[[i]] <- matrix(NA, nrow = 0, ncol = n.var, dimnames = list(NULL, dimnames(x[[1]])[[2]]))
    newobjects[[i]] <- rbind(x[[i]], y[[i]])
    newobjects[[i]] <- mcmc(newobjects[[i]])
  }
  mcmc.list(newobjects)
}

find.max.gelman <- function(samples, index = NULL){

  if(!is.null(index)){
    samples <- lapply(samples, function(x){ x[,index]})
  }
  samples <- lapply(samples, function(x) { x[,colSums(abs(x)) != 0] })

  max(gelman.diag(samples, multivariate = FALSE)$psrf[,1]) #look at point estimate instead of 95% C.I.
}
