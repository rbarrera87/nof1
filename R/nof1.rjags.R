nof1.rjags <- function(nof1){

  response <- nof1$response
  code <- if(response == "normal"){
    nof1.normal.rjags(nof1)
  } else if(response == "ordinal"){
    nof1.ordinal.rjags(nof1)
  } else if(response == "binomial"){
    nof1.binomial.rjags(nof1)
  } else if(response == "poisson"){
    nof1.poisson.rjags(nof1)
  }
  return(code)
}

nof1.normal.rjags <- function(nof1){

  with(nof1, {

  code <- paste0("model{")
  code <- paste0(code,
                 "\n\tfor (i in 1:", nobs, ") {",
                 "\n\t\tm[i] <- mu[i] #+ rho * epsilon[i]",
                 "\n\t\tmu[i] <- alpha")

  for(i in Treat.name){
    code <- paste0(code, " + beta_", i, "*Treat_", i, "[i]")
  }

  # if(!is.null(knots)){
  #   for(j in 1:ncol(BS)){
  #     code <- paste0(code, "# + gamma", j, "* BS[i,", j, "]")
  #   }
  # }

  code <- paste0(code,
                 "\n\t\tY[i] ~ dnorm(m[i], prec)",
                 "\n\t}",
                 "\n\t#e0 ~ dnorm(0, (1-rho^2)*prec)",
                 "\n\t#epsilon[1] <- e0",
                 "\n\t#for(i in 2:", nobs, "){",
                 "\n\t\t#epsilon[i] <- Y[i-1] - mu[i-1]",
                 "\n\t#}",
                 "\n\talpha ~ ", alpha.prior[[1]], "(", alpha.prior[[2]], ",", alpha.prior[[3]], ")",
                 "\n\t#rho ~ ", rho.prior[[1]], "(", rho.prior[[2]], ",", rho.prior[[3]], ")"
                 )

  for(i in Treat.name){
    code <- paste0(code, "\n\tbeta_", i, " ~ ", beta.prior[[1]], "(", beta.prior[[2]], ",", beta.prior[[3]], ")")
  }

  # if(!is.null(knots)){
  #   for(j in 1:ncol(BS)){
  #     code <- paste0(code, "\n\tgamma", j, " ~ ", gamma.prior[[1]], "(", gamma.prior[[2]], ",", gamma.prior[[3]], ")")
  #   }
  # }

  code <- paste0(code, nof1.hy.prior.rjags(hy.prior), "\n}")
  return(code)
  })
}




nof1.binomial.rjags <- function(nof1){

  with(nof1, {

  code <- paste0("model{")
  code <- paste0(code,
                 "\n\tfor (i in 1:", nobs, ") {",
                 "\n\t\tlogit(p[i]) <- alpha")

  for(i in Treat.name){
    code <- paste0(code, " + beta_", i, "*Treat_", i, "[i]")
  }

  # if(!is.null(knots)){
  #   for(j in 1:ncol(BS)){
  #     code <- paste0(code, " + gamma", j, "* BS[i,", j, "]")
  #   }
  # }

  code <- paste0(code,
                 "\n\t\tY[i] ~ dbern(p[i])",
                 "\n\t}",
                 "\n\talpha ~ ", alpha.prior[[1]], "(", alpha.prior[[2]], ",", alpha.prior[[3]], ")")

  for(i in Treat.name){
    code <- paste0(code, "\n\tbeta_", i, " ~ ", beta.prior[[1]], "(", beta.prior[[2]], ",", beta.prior[[3]], ")")
  }
  
  code <- paste0(code, "\n\tp_", baseline, " <- ilogit(alpha)")
  for(i in Treat.name){
    code <- paste0(code, "\n\tp_", i, " <- ilogit(alpha + beta_", i, ")")
  }
  
  # if(!is.null(knots)){
  #   for(j in 1:ncol(BS)){
  #     code <- paste0(code, "\n\tgamma", j, " ~ ", gamma.prior[[1]], "(", gamma.prior[[2]], ",", gamma.prior[[3]], ")")
  #   }
  # }
  code <- paste0(code, "\n}")
  
  return(code)
  })

}


nof1.poisson.rjags <- function(nof1){

  with(nof1, {

  code <- paste0("model{")
  code <- paste0(code,
                 "\n\tfor (i in 1:", nobs, ") {",
                 "\n\t\tlog(lambda[i]) <- alpha") # + epsilon[i]")

  for(i in Treat.name){
    code <- paste0(code, " + beta_", i, "*Treat_", i, "[i]")
  }

  # if(!is.null(knots)){
  #   for(j in 1:ncol(BS)){
  #     code <- paste0(code, " + gamma", j, "* BS[i,", j, "]")
  #   }
  # }

  code <- paste0(code,
                 "\n\t\tY[i] ~ dpois(lambda[i])",
                 "\n\t}",
                 "\n\t#e0 ~ dnorm(0, (1- rho^2) * prec)",
                 "\n\t#epsilon[1] <- e0",
                 "\n\t#for(i in 2:", nobs, "){",
                 "\n\t\t#epsilon[i] ~ dnorm(epsilon.m[i], prec)",
                 "\n\t\t#epsilon.m[i] <- rho * epsilon[i-1]",
                 "\n\t#}",
                 "\n\talpha ~ ", alpha.prior[[1]], "(", alpha.prior[[2]], ",", alpha.prior[[3]], ")",
                 "\n\t#rho ~ ", rho.prior[[1]], "(", rho.prior[[2]], ",", rho.prior[[3]], ")")

  for(i in Treat.name){
    code <- paste0(code, "\n\tbeta_", i, " ~ ", beta.prior[[1]], "(", beta.prior[[2]], ",", beta.prior[[3]], ")")
  }

  # if(!is.null(knots)){
  #   for(j in 1:ncol(BS)){
  #     code <- paste0(code, "\n\tgamma", j, " ~ ", gamma.prior[[1]], "(", gamma.prior[[2]], ",", gamma.prior[[3]], ")")
  #   }
  # }
  #code <- paste0(code, nof1.hy.prior.rjags(hy.prior), "\n}")
  code <- paste0(code, "\n}")
  return(code)
  })
}


nof1.ordinal.rjags <- function(nof1){

  with(nof1, {

    code <- paste0("model{")
    code <- paste0(code,
                   "\n\tfor (i in 1:", nobs, ") {",
                   "\n\t\tY[i] ~ dcat(p[i,])",
                   "\n\t\tp[i,1] <- 1 - Q[i,1]",
                   "\n\t\tfor(r in 2:", ncat-1, ") {",
                   "\n\t\t\tp[i,r] <- Q[i,r-1] - Q[i,r]",
                   "\n\t\t}",
                   "\n\t\tp[i,", ncat, "] <- Q[i,", ncat-1, "]",
                   "\n\t\tfor(r in 1:", ncat-1, ") {",
                   "\n\t\t\tlogit(Q[i,r]) <- -c[r]") # + epsilon[i]")

    for(i in Treat.name){
      code <- paste0(code, " + beta_", i, "*Treat_", i, "[i]")
    }

    # if(!is.null(knots)){
    #   for(j in 1:ncol(BS)){
    #     code <- paste0(code, " + gamma", j, "* BS[i,", j, "]")
    #   }
    # }

    code <- paste0(code,
                   "\n\t\t}",
                   "\n\t}",
                   "\n\t#e0 ~ dnorm(0, (1-rho^2)*prec)",
                   "\n\t#epsilon[1] <- e0",
                   "\n\t#for(i in 2:", nobs, "){",
                   "\n\t\t#epsilon[i] ~ dnorm(epsilon.m[i], prec)",
                   "\n\t\t#epsilon.m[i] <- rho * epsilon[i-1]",
                   "\n\t#}",
                   "\n\tfor(i in 2:", ncat-1, ") {",
                   "\n\t\tdc[i] ~ ", dc.prior[[1]], "(", dc.prior[[2]], ",", dc.prior[[3]], ")",
                   "\n\t}",
                   "\n\tc[1] <- dc[1]",
                   "\n\tfor (i in 2:", ncat-1, ") {",
                   "\n\t\tc[i] <- c[i-1] + dc[i]",
                   "\n\t}",
                   "\n\tdc[1] ~ ", c1.prior[[1]], "(", c1.prior[[2]], ",", c1.prior[[3]], ")",
                   "\n\t#rho ~ ", rho.prior[[1]], "(", rho.prior[[2]], ",", rho.prior[[3]], ")")

    for(i in Treat.name){
      code <- paste0(code, "\n\tbeta_", i, " ~ ", beta.prior[[1]], "(", beta.prior[[2]], ",", beta.prior[[3]], ")")
    }

    # if(!is.null(knots)){
    #   for(j in 1:ncol(BS)){
    #     code <- paste0(code, "\n\tgamma", j, " ~ ", gamma.prior[[1]], "(", gamma.prior[[2]], ",", gamma.prior[[3]], ")")
    #   }
    # }

    #code <- paste0(code, nof1.hy.prior.rjags(hy.prior), "\n}")
    code <- paste0(code, "\n}")
    return(code)
  })
}



nof1.hy.prior.rjags <- function(hy.prior){

  code <- ""
  distr <- hy.prior[[1]]
  if (distr == "dunif") {
    code <- paste0(code,
                   "\n\tsd ~ dunif(", hy.prior[[2]], ", ", hy.prior[[3]], ")",
                   "\n\tprec <- pow(sd,-2)",
                   "\n\tlogprec <- log(prec)")
  } else if(distr == "dgamma"){
    code <- paste0(code,
                   "\n\tsd <- pow(prec, -0.5)",
                   "\n\tprec ~ dgamma(", hy.prior[[2]], ", ", hy.prior[[3]], ")",
                   "\n\tlogprec <- log(prec)")
  } else if(distr == "dhnorm"){
    code <- paste0(code,
                   "\n\tsd ~ dnorm(", hy.prior[[2]], ", ", hy.prior[[3]], ")T(0,)",
                   "\n\tprec <- pow(sd, -2)",
                   "\n\tlogprec <- log(prec)")
  }
  return(code)
}


