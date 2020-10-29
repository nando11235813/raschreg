sim_rasch<-function(n,
                    delta  = NULL,
                    alpha  = NULL,
                    gamma  = NULL,
                    reg    = NULL,
                    beta   = NULL,
                    ability= FALSE){
  if (is.null(delta)) delta <- seq(-2, 2, length = 7)
  J <- length(delta)
  if (is.null(alpha)) alpha <- rep(1, J)
  if (is.null(gamma)) gamma <- rep(0, J)
  
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
