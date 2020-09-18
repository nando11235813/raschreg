raschlikLA_skew<-function(par, X){
  J  <- ncol(X)
  pat<- X %*% 10^seq(0, J - 1)
  X  <- X[which(!duplicated(pat)), ]
  np <- merge(data.frame(pat = unique(pat)),
              as.data.frame(table(pat)),
              by   = 'pat',
              sort = FALSE)$Freq
  alphas <- rep(1, J)
  deltas <- par[1:J]
  nu     <- par[J + 1]
  
	# maximum  on each row
	bmax   <- apply(X,
	                1,
	                function(fila)optimize(hbc_skew,
	                                       interval = c(-4, 4),
	                                       x        = fila,
	                                       d        = deltas,
	                                       a        = alphas,
	                                       nu       = nu,
	                                       tol      = 1e-4,
	                                       maximum  = TRUE)$maximum)
  # log-likelihood value hmax-0.5*log(-h''max)
	ll     <- raschlik_skew(par, X, bmax, a = alphas, np = np, nu = nu)
	return(-ll)
}
