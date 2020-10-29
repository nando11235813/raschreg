sim_rasch<-function(n,
                    delta  = seq(-1.5,1.5,length=J),
                    alpha  = rep(1,J),
                    gamma  = rep(0,J),
                    reg    = NULL,
                    beta   = NULL,
                    ability= FALSE){
  X <- matrix(0, n, J)
  if(!is.null(beta) & !is.null(reg)) {
    mu <- reg%*%beta
  } else {
    mu<-rep(0, n)
  }
  if(any(alpha <= 0)) stop('discrimination parameters must be positive')
  if(!is.null(reg) & is.null(beta)) stop('regression parameters must be specified')
  if(is.null(reg) & !is.null(beta)) stop('regression covariates must be specified')
  
  # ability simulation
  theta <- rnorm(n, mu, 1)
  
  # item simulation
  for (i in 1:n) {
    for (j in 1:J) {
      eta_ij <- alpha[j]*(theta[i] - delta[j])
      p_ij   <- exp(eta_ij)/(1 + exp(eta_ij))
      X[i,j] <- rbinom(1, 1, gamma[j] + (1 - gamma[j])*p_ij)
    }
  }  
  colnames(X) <- paste('item', 1:J, sep = '')
  if (ability == TRUE) X <- cbind(X, theta)
  return(X)
}
