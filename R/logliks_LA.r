# log likelihoods

####################################
# Models without linear predictors #
####################################

# Rasch Model
raschlikLA <- function(par, X, fixed = NULL){
  J   <- ncol(X)
  pat <- X %*% 10^seq(0, J - 1)
  X   <- X[which(!duplicated(pat)), ,drop=FALSE]
  np  <- merge(data.frame(pat = unique(pat)),
               as.data.frame(table(pat)),
               by = 'pat', sort = FALSE)$Freq
  alphas <- rep(1, J)
  
  # constraints
  if (!is.null(fixed)){
    if (length(fixed) != J) stop("Wrong length in 'fixed'")
    fix_d <- which(!is.na(fixed))
    par[fix_d] <- fixed[fix_d]
  }
    
	# maximum  on each row
	bmax <- apply(X, 1,
	              function(fila)optimize(hbc,
	                                     interval = c(-4, 4),
	                                     x   = fila,
	                                     d   = par,
	                                     a   = alphas,
	                                     tol = 1e-4,
	                                     maximum = TRUE)$maximum)
  # log-likelihood value hmax-0.5*log(-h''max)
	ll <- raschlik(d    = par,
	               X    = X,
	               bmax = bmax,
	               a    = alphas,
	               np   = np)
	return(-ll)
}

# Rasch Model w/discrimination parameter
raschdlikLA <- function(par, X, fixed = NULL){
  J     <- ncol(X)
  pat   <- X %*% 10^seq(0, J - 1)
  X     <- X[which(!duplicated(pat)), ,drop=FALSE]
  np    <- merge(data.frame(pat = unique(pat)),
                 as.data.frame(table(pat)),
                 by = 'pat',
                 sort = FALSE)$Freq
  delta <- par[1:J]
  alpha <- rep(par[J + 1], J)

  # constraints
  if (!is.null(fixed)){
    if (length(fixed) != (J + 1)) stop("Wrong length in 'fixed'")
    fix_d <- which(!is.na(fixed))
    delta[fix_d] <- fixed[fix_d]
  }
  
  # maximum on each row of X
  bmax <- apply(X, 1,
                function(rowi) optimize(hbc,
                                        interval = c(-4,4),
                                        x   = rowi,
                                        d   = delta,
                                        a   = alpha,
                                        tol = 1e-4,
                                        maximum = TRUE)$maximum)
  ll   <- raschlik(d    = delta, 
                   X    = X,
                   bmax = bmax,
                   a    = alpha,
                   np   = np)
  return(-ll)
}

# Two Parameter Logistic Model
irt2plikLA <- function(par, X, fixed = NULL){

  J     <- ncol(X)
  delta <- par[seq(J)]
  alpha <- par[-seq(J)]

  pat   <- apply(X, 1, paste, collapse = '')
  X     <- X[which(!duplicated(pat)), ,drop=FALSE]
  np    <- merge(data.frame(pat = unique(pat)),
                as.data.frame(table(pat)),
                by   = 'pat',
                sort = FALSE)$Freq
  # constraints
  if (!is.null(fixed)){
    if (length(fixed) != (2*J)) stop("Wrong length in 'fixed'")
    fix_d <- which(!is.na(fixed))
    par[fix_d] <- fixed[fix_d]
  }
  
  # maximum on each X row
  bmax <- apply(X, 1,
                function(rowi) optimize(hbc,
                                        interval = c(-3, 3),
                                        x        = rowi,
                                        d        = delta,
                                        a        = alpha,
                                        tol      = 1e-4,
                                        maximum  = TRUE)$maximum)
  # log-likelihood value hmax-0.5*log(-h''max)
  ll <- raschlik(d    = delta,
                 X    = X,
                 bmax = bmax,
                 a    = alpha,
                 np   = np)
  return(-ll)
}


#################################
# Models with linear predictors #
#################################

# Rasch Regression Model
raschreglikLA <- function(par, X, Z){
	J      <- ncol(X)
	deltas <- par[seq(J)]
	beta   <- par[-seq(J)]
	alfas  <- rep(1, J)

	# maximum of each row
	bmax <- apply(data.frame(X,Z),
	              1,
	              function(fila)optimize(huc,
	                                     interval = c(-4, 4),
	                                     x        = fila[seq(J)],
	                                     d        = deltas,
	                                     a        = alfas,
	                                     z        = fila[-seq(J)],
	                                     beta     = beta,
	                                     tol      = 1e-4,
	                                     maximum  = TRUE)$maximum)
 	# log-likelihood value hmax-0.5*log(-h''max)
	ll <- raschreglik(d    = deltas,
	                  a    = alfas,
	                  X    = X,
	                  bmax = bmax,
	                  Z    = Z,
	                  beta = beta)
	return(-ll)
}

# Rasch Regression Model w/discrimination parameter
raschdreglikLA <- function(par, X, Z){
  J      <- ncol(X)
  deltas <- par[seq(J)]
  alphas <- rep(par[J + 1], J)
  betas  <- par[-seq(J + 1)]
  
  # maximum on each X row
  bmax<-apply(data.frame(X, Z),
              1,
              function(rowi) optimize(huc,
                                      interval = c(-3, 3),
                                      x        = rowi[seq(J)],
                                      d        = deltas,
                                      a        = alphas,
                                      beta     = betas,
                                      z        = rowi[-seq(J)],
                                      tol      = 1e-4,
                                      maximum  = TRUE)$maximum)
  # log-likelihood value hmax-0.5*log(-h''max)
  ll <- raschreglik(d    = deltas,
                    a    = alphas,
                    X    = X,
                    bmax = bmax,
                    Z    = Z,
                    beta = betas)
  return(-ll)
}

# Two Parameter Regression Logistic Model
irt2preglikLA <- function(par, X, Z){

  J     <- ncol(X)
  p     <- ncol(Z)
  delta <- par[seq(J)]
  alpha <- par[(J + 1):(2*J)]
  beta  <- par[(2*J + 1):(2*J + p)]
  
  # maximum on each X row
  bmax <- apply(data.frame(X, Z),
                1,
                function(rowi) optimize(huc,
                                        interval = c(-3, 3),
                                        x        = rowi[seq(J)],
                                        d        = delta,
                                        a        = alpha,
                                        beta     = beta,
                                        z        = rowi[-seq(J)],
                                        tol      = 1e-4,
                                        maximum  = TRUE)$maximum)
  # log-likelihood value hmax-0.5*log(-h''max)
  ll <- raschreglik(d    = delta,
                    a    = alpha,
                    X    = X,
                    bmax = bmax,
                    Z    = Z,
                    beta = beta)
  return(-ll)
}