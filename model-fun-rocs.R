evsdt_llfun <- function(par, data, return = "ll", ...){
  
  dp <- par[1] ## signal mean = dp/2, noise mean = -dp/2
  c <- par[2] 
  sd <- 1
  
  nside <- length(data)/2
  nc <- nside/2 - 1
  
  cl <- rev(c - cumsum(par[3:(2+nc)]))
  cr <- c + cumsum(par[(3+nc):(2+2*nc)])
  allc <- c(cl, c, cr)
  
  psignal <- vector("numeric", nside)
  pnoise <- vector("numeric", nside)
  
  psignal[1] <- pnorm(allc[1], mean = dp/2, sd = sd)
  pnoise[1] <- pnorm(allc[1], mean = -dp/2, sd = 1)
  for (i in 1+seq_len(nside-2)) {
    psignal[i] <- pnorm(allc[i], mean = dp/2, sd = sd) - pnorm(allc[i-1], mean = dp/2, sd = sd)
    pnoise[i] <- pnorm(allc[i], mean = -dp/2, sd = 1) - pnorm(allc[i-1], mean = -dp/2, sd = 1)
  }
  psignal[nside] <- 1 - pnorm(allc[nside-1], mean = dp/2, sd = sd)
  pnoise[nside] <- 1 - pnorm(allc[nside-1], mean = -dp/2, sd = 1)
  
  e <- c(psignal, pnoise)
  if (return == "pred") return(e)
  
  LL <- -sum(data[data!=0]*log(e[data!=0]))
  return(LL)
}

uvsdt_llfun <- function(par, data, return = "ll", ...){
  
  dp <- par[1] ## signal mean = dp/2, noise mean = -dp/2
  c <- par[2] 
  sd <- par[3]
  
  nside <- length(data)/2
  nc <- nside/2 - 1
  
  cl <- rev(c - cumsum(par[4:(3+nc)]))
  cr <- c + cumsum(par[(4+nc):(3+2*nc)])
  allc <- c(cl, c, cr)
  
  psignal <- vector("numeric", nside)
  pnoise <- vector("numeric", nside)
  
  psignal[1] <- pnorm(allc[1], mean = dp/2, sd = sd)
  pnoise[1] <- pnorm(allc[1], mean = -dp/2, sd = 1)
  for (i in 1+seq_len(nside-2)) {
    psignal[i] <- pnorm(allc[i], mean = dp/2, sd = sd) - pnorm(allc[i-1], mean = dp/2, sd = sd)
    pnoise[i] <- pnorm(allc[i], mean = -dp/2, sd = 1) - pnorm(allc[i-1], mean = -dp/2, sd = 1)
  }
  psignal[nside] <- 1 - pnorm(allc[nside-1], mean = dp/2, sd = sd)
  pnoise[nside] <- 1 - pnorm(allc[nside-1], mean = -dp/2, sd = 1)
  
  e <- c(psignal, pnoise)
  if (return == "pred") return(e)
  
  LL <- -sum(data[data!=0]*log(e[data!=0]))
  return(LL)
}

mixsdt_llfun <- function(par, data, return = "ll", ...){
  
  dp <- par[1] ## signal mean = dp/2, noise mean = -dp/2
  c <- par[2] 
  lambda <- par[3]
  
  nside <- length(data)/2
  nc <- nside/2 - 1
  
  cl <- rev(c - cumsum(par[4:(3+nc)]))
  cr <- c + cumsum(par[(4+nc):(3+2*nc)])
  allc <- c(cl, c, cr)
  
  psignal <- vector("numeric", nside)
  pnoise <- vector("numeric", nside)
  
  psignal[1] <- lambda * pnorm(allc[1], mean = dp/2, sd = 1) + (1- lambda)*pnorm(allc[1], mean = -dp/2, sd = 1)
  pnoise[1] <- pnorm(allc[1], mean = -dp/2, sd = 1) 
  for (i in 1+seq_len(nside-2)) {
    psignal[i] <- lambda * (pnorm(allc[i], mean = dp/2, sd = 1) - pnorm(allc[i-1], mean = dp/2, sd = 1)) + (1- lambda)*(pnorm(allc[i], mean = -dp/2, sd = 1) - pnorm(allc[i-1], mean = -dp/2, sd = 1))
    pnoise[i] <- pnorm(allc[i], mean = -dp/2, sd = 1) - pnorm(allc[i-1], mean = -dp/2, sd = 1)
  }
  psignal[nside] <- lambda * (1 - pnorm(allc[nside-1], mean = dp/2, sd = 1)) + (1-lambda)*(1 - pnorm(allc[nside-1], mean = -dp/2, sd = 1))
  pnoise[nside] <- 1 - pnorm(allc[nside-1], mean = -dp/2, sd = 1)
  
  e <- c(psignal, pnoise)
  if (return == "pred") return(e)
  
  LL <- -sum(data[data!=0]*log(e[data!=0]))
  return(LL)
}


gumbelmin <- function(x, mu) {
  return(1 - extraDistr::pgumbel(-x,-mu, 1))
}

gumbelmin_trunc <- function(x, mu, a = -Inf, b = Inf) {
  if (!is.finite(a) & is.finite(b)) {
    return(gumbelmin(x, mu)/gumbelmin(b, mu))
  } else if (is.finite(a) & !is.finite(b)) {
    return((gumbelmin(x, mu) - gumbelmin(a, mu))/(1-gumbelmin(a, mu)))
  } else if (is.finite(a) & is.finite(b)) {
    return((gumbelmin(x, mu) - gumbelmin(a, mu))/(gumbelmin(b, mu)-gumbelmin(a, mu)))
  } else {
    return(gumbelmin(x, mu))
  }
}

gumbelmin_llfun <- function(par, data, return = "ll", ...){
  
  dp <- par[1] ## signal mean = dp/2, noise mean = -dp/2
  c <- par[2] 
  
  nside <- length(data)/2
  nc <- nside/2 - 1
  
  cl <- rev(c - cumsum(par[3:(2+nc)]))
  cr <- c + cumsum(par[(3+nc):(2+2*nc)])
  allc <- c(cl, c, cr)
  
  psignal <- vector("numeric", nside)
  pnoise <- vector("numeric", nside)
  
  psignal[1] <- gumbelmin(allc[1], mu = dp/2)
  pnoise[1] <- gumbelmin(allc[1], mu = -dp/2)
  for (i in 1+seq_len(nside-2)) {
    psignal[i] <- gumbelmin(allc[i], mu = dp/2) - gumbelmin(allc[i-1], mu = dp/2)
    pnoise[i] <- gumbelmin(allc[i], mu = -dp/2) - gumbelmin(allc[i-1], mu = -dp/2)
  }
  psignal[nside] <- 1 - gumbelmin(allc[nside-1], mu = dp/2)
  pnoise[nside] <- 1 - gumbelmin(allc[nside-1], mu = -dp/2)
  
  e <- c(psignal, pnoise)
  if (return == "pred") return(e)
  
  LL <- -sum(data[data!=0]*log(e[data!=0]))
  return(LL)
}
