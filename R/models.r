##########
# Models #
##########

# Rasch Model
rasch <- function(items, init = NULL, fixed = NULL){

  if ('data.frame' %in% class(items)) items <- as.matrix(items)
  # initial values
	J <- ncol(items)
	if (is.null(init)) init <- rep(0, J)

	# NA checking
	if(any(is.na(items))) {
		na    <- apply(is.na(items), 1, sum)
		items <- items[-which(na != 0), ]
		print(paste(sum(na != 0), ' rows were removed', sep = ''))
	}
  par <- nlminb(start     = init,
                objective = raschlikLA,
                X         = items,
                control   = list(rel.tol = 1e-5,
                                 x.tol   = 1e-5),
                fixed     = fixed)
  
 	# approximate observed information matrix
	H <- hessian(fun   = raschlikLA,
	             param = par$par,
	             X     = items,
	             fun0  = par$objective,
	             fixed = fixed)
	if (!is.null(fixed)){
	  fix_d <- which(!is.na(fixed))
	  H <- H[-fix_d, -fix_d]
	}
	V  <- solve(H)

	# constraints
	unc <- 1:J
	if (!is.null(fixed)){
	  unc[which(!is.na(fixed))] <- NA
	}
	if (any(is.na(unc))){
	  unc <- unc[!is.na(unc)]
	  V1 <- matrix(NA, J, J)
	  V1[unc, unc] <- V
	  V <- V1
	  rm(V1)
	}
	
	rownames(V) <- colnames(V) <- paste('delta',
	                                    colnames(items),
	                                    sep = '_')
	se <- sqrt(diag(V))
	
	loglik <- par$objective
	iter   <- par$iterations
	dof    <- nrow(items) - length(par$par)
	deltas <- par$par
	delta  <- data.frame(deltas,
	                     se,
	                     deltas/se,
	                     2*(1 - pt(abs(deltas/se), df = dof)))
	colnames(delta) <- c('Estimate',
	                     'Std. Error',
	                     't value',
	                     'Pr(|>t|)')
	rownames(delta) <- rownames(V)

	call <- match.call()
	mod  <- list(call   = call,
	             coef   = delta,
	             iter   = iter,
	             loglik = -loglik,
	             vcov   = V,
	             items  = items)
	class(mod) <- append(class(mod), 'rasch')
	return(mod)
}

# Rasch Model w/discrimination parameter
raschd <- function(items, init = NULL, fixed = NULL){
  if ('data.frame' %in% class(items)) items <- as.matrix(items)
  # initial values
  J  <- ncol(items)
  if (is.null(init)) init <- rep(0, J +1)
  
  # NA checking
  if(any(is.na(items))) {
    na    <- apply(is.na(items), 1, sum)
    items <- items[-which(na != 0), ]
    print(paste(sum(na != 0),' rows were removed', sep = ''))
  }
  par <- nlminb(start     = init,
                objective = raschdlikLA,
                X         = items,
                control   = list(rel.tol = 1e-5,
                                 x.tol   = 1e-5),
                fixed     = fixed)
  
  # approximate observed information matrix
  H <- hessian(fun   = raschdlikLA,
               param = par$par,
               X     = items,
               fun0  = par$objective,
               fixed = fixed)

	if (!is.null(fixed)){
	  fix_d <- which(!is.na(fixed))
	  H <- H[-fix_d, -fix_d]
	}
	V  <- solve(H)
	
	# constraints
	unc <- 1:(J + 1)
	if (!is.null(fixed)){
	  unc[which(!is.na(fixed))] <- NA
	}
	if (any(is.na(unc))){
	  unc <- unc[!is.na(unc)]
	  V1 <- matrix(NA, J +1, J + 1)
	  V1[unc, unc] <- V
	  V <- V1
	  rm(V1)
	}
	# delta method covariances
	h <- c(rep(1, J), exp(par$par[J +1]))
	V <- outer(h, h)*V
  
  rownames(V) <- colnames(V) <- c(colnames(items), 'alpha')
  se <- sqrt(diag(V))
  # correcting discrimination parameter
  par$par[J + 1] <- exp(par$par[J + 1])
  
  loglik <- par$objective
  iter   <- par$iterations
  dof    <- nrow(items) - length(par$par)
  parH0  <- rep(c(0, 1), c(J, 1))
  delta  <- data.frame(par$par,
                       se,
                       (par$par - parH0)/se,
                       2*(1 - pt(abs((par$par - parH0)/se), dof)))
  rownames(delta) <- c(paste('delta', colnames(items), sep = '_'), 'alpha')
  colnames(delta) <- c('Estimate',
                       'Std. Error',
                       't value',
                       'Pr(|>t|)')
  
  mod <- list(call   = match.call(),
              coef   = delta,
              iter   = iter,
              loglik = -loglik,
              vcov   = V,
              items  = items)
  class(mod) <- append(class(mod), 'rasch')
  return(mod)  
}

# Two parameter logistic Model
irt2p <- function(items, init = NULL, fixed = NULL){
  if ('data.frame' %in% class(items)) data <- as.matrix(items)
  # initial values
  J  <- ncol(items)
  if (is.null(init)) init <- rep(0, 2*J)

  # NA checking
  if(any(is.na(items))) {
    na    <- apply(is.na(items), 1, sum)
    items <- items[ -which(na!=0), ]
    print(paste(sum(na != 0), ' rows were removed', sep = ''))
  }
  par <- nlminb(init,
                irt2plikLA,
                X       = items,
                control = list(rel.tol = 1e-5,
                             x.tol     = 1e-5),
                fixed     = fixed)
 
  # approximate observed information matrix
  H <- hessian(fun   = irt2plikLA,
               param = par$par,
               X     = items,
               fun0  = par$value,
               fixed = fixed)
  if (!is.null(fixed)){
	  fix_d <- which(!is.na(fixed))
	  H <- H[-fix_d, -fix_d]
	}
  V <- solve(H)
	
  # constraints
	unc <- 1:(2*J)
	if (!is.null(fixed)){
	  unc[which(!is.na(fixed))] <- NA
	}
	if (any(is.na(unc))){
	  unc <- unc[!is.na(unc)]
	  V1 <- matrix(NA, 2*J, 2*J)
	  V1[unc, unc] <- V
	  V <- V1
	  rm(V1)
	}
	
	# delta method covariances
	a_p <- (J+1):(2*J)
 	h   <- c(rep(1, J), exp(par$par[a_p]))
	V   <- outer(h, h)*V
	
  rownames(V) <- colnames(V) <- paste(rep(c('delta' , 'alpha'), each = J), colnames(items), sep = '_')
  se <- sqrt(diag(V))
  # correcting discrimination parameters
  par$par[a_p] <- exp(par$par[a_p])

  loglik <- par$objective
  iter   <- par$iterations
  dof    <- nrow(items) - length(par$par)
  parH0  <- rep(c(0, 1), each = J)
  delta  <- data.frame(par$par,
                       se,
                       (par$par - parH0)/se,
                       2*(1 - pt(abs((par$par - parH0)/se),df = dof)))
  colnames(delta) <- c('Estimate',
                       'Std. Error',
                       't value',
                       'Pr(|>t|)')
  rownames(delta) <- rownames(V)
  
  call <- match.call()
  mod  <- list(call   = call,
               coef   = delta,
               iter   = iter,
               loglik = -loglik,
               vcov   = V,
               items  = items)
  class(mod) <- append(class(mod), 'rasch')
  return(mod) 
}

#####################
# Regression Models #
#####################

# Rasch Regression Model
raschreg <- function(items, f_reg, z_reg, init = NULL, fixed = NULL){
  if ('data.frame' %in% class(items))   items <- as.matrix(items)
  if (! 'data.frame' %in% class(z_reg)) z_reg <- as.data.frame(z_reg)
  z_reg <- as.matrix(model.frame(f_reg,
                                 z_reg,
                                 na.action = 'na.pass'))
  J <- ncol(items)
	p <- ncol(z_reg)
	# valores iniciales
	if (is.null(init)) init <- rep(0, J + p)

	# NA checking
	if(any(is.na(data.frame(items, z_reg)))) {
		na    <- apply(is.na(data.frame(items, z_reg)), 1, sum)
		items <- items[-which(na != 0), ]
		z_reg <- z_reg[-which(na != 0), ]
		print(paste(sum(na != 0), ' rows were removed', sep = ''))
	}

	# estimation
	par <- nlminb(start     = init,
	              objective = raschreglikLA,
	              X         = items,
	              Z         = z_reg,
	              control   = list(rel.tol = 1e-5,
	                               x.tol = 1e-4),
	              fixed = fixed)
	
	# approximate observed information matrix
	H <- hessian(fun   = raschreglikLA,
	             param = par$par,
	             X     = items,
	             Z     = z_reg,
	             fun0  = par$objective,
	             fixed = fixed)

	if (!is.null(fixed)){
	  fix_d <- which(!is.na(fixed))
	  H <- H[-fix_d, -fix_d]
	}
	V  <- solve(H)

	# constraints
	unc <- 1:(J + p)
	if (!is.null(fixed)){
	  unc[which(!is.na(fixed))] <- NA
	}
	if (any(is.na(unc))){
	  unc <- unc[!is.na(unc)]
	  V1 <- matrix(NA, J + p, J + p)
	  V1[unc, unc] <- V
	  V <- V1
	  rm(V1)
	}

	colnames(V) <- rownames(V) <- c(paste('delta', colnames(items), sep = '_'),
	                                paste('beta', colnames(z_reg), sep = '_'))
	se    <- sqrt(diag(V))
	dof   <- nrow(items) - length(par$par)
	coeff <- data.frame(par$par,
	                    se,
	                    par$par/se,
	                    2*(1 - pt(abs(par$par/se), df = dof)))
	rownames(coeff) <- rownames(V)
	colnames(coeff) <- c('Estimate',
	                     'Std. Error',
	                     't value',
	                     'Pr(|>t|)')
	delta <- coeff[seq(J), ]
	beta  <- coeff[-seq(J), ]
	
	loglik <- par$objective
	iter   <- par$iterations
	
	mod        <- list(call    = match.call(),
	                   coef    = delta,
	                   beta    = beta,
	                   iter    = iter,
	                   loglik  = -loglik,
	                   vcov    = V,
	                   items   = items,
	                   linpred = z_reg)
	class(mod) <- append(class(mod), 'rasch')
	return(mod)
}

# Rasch Regression Model w/discrimination parameter
raschdreg <- function(items, f_reg, z_reg, init = NULL){
  if ('data.frame' %in%  class(items))  data  <- as.matrix(items)
  if (! 'data.frame' %in% class(z_reg)) z_reg <- as.data.frame(z_reg)
  z_reg <- as.matrix(model.frame(f_reg,
                                 z_reg,
                                 na.action = 'na.pass'))
  J <- ncol(items)
  p <- ncol(z_reg)

  # initial values
  if (is.null(init)) init<-c(rep(0, J), 1, rep(0, p))
  lo <- c(rep(-Inf, J), 0, rep(-Inf, p))
  
  # NA checking
  if(any(is.na(data.frame(items, z_reg)))) {
    na    <- apply(is.na(data.frame(items, z_reg)), 1, sum)
    items <- items[-which(na != 0), ]
    z_reg <- z_reg[-which(na != 0), ]
    print(paste(sum(na != 0), ' rows were removed', sep = ''))
  }
  
  # estimation
  par <- nlminb(start     = init,
                objective = raschdreglikLA,
                lower     = lo,
                X         = items,
                Z         = z_reg,
                control   = list(rel.tol = 1e-5, x.tol = 1e-4))
  
  # approximate observed information matrix
  H <- hessian(fun   = raschdreglikLA,
               param = par$par,
               X     = items,
               Z     = z_reg,
               fun0  = par$objective)
  V <-solve(H)
  colnames(V) <- rownames(V) <- c(paste('delta', colnames(items), sep='_'), 
                                  'alpha',
                                  paste('beta', colnames(z_reg), sep = '_'))
  se    <- sqrt(diag(V))
  dof   <- nrow(items) - length(par$par)
  coeff <- data.frame(par$par,
                      se,
                      (par$par-init)/se,
                      2*(1 - pt(abs((par$par - init)/se), df = dof)))
  rownames(coeff) <- rownames(V)
  colnames(coeff) <- c('Estimate',
                       'Std. Error',
                       't value',
                       'Pr(|>t|)')
  delta <- coeff[seq(J + 1), ]
  beta  <- coeff[-seq(J + 1), ]
  
  loglik <- par$objective
  iter   <- par$iterations
  
  mod        <- list(call    = match.call(),
                     coef    = delta,
                     beta    = beta,
                     iter    = iter,
                     loglik  = -loglik, 
                     vcov    = V,
                     items   = items,
                     linpred = z_reg)
  class(mod) <- append(class(mod), 'rasch')
  return(mod)
}

# Two parameter logistic Regression Model
irt2preg <- function(items, f_reg, z_reg, init = NULL){
  if ('data.frame' %in% class(items))   data  <- as.matrix(items)
  if (! 'data.frame' %in% class(z_reg)) z_reg <- as.data.frame(z_reg)
  z_reg <- as.matrix(model.frame(f_reg,
                                 z_reg,
                                 na.action = 'na.pass'))
  J <- ncol(items)
  p <- ncol(z_reg) 

  lo <- rep(c(-Inf, 0 ,-Inf), c(J, J, p))
  # initial values
  if (is.null(init)) init <- rep(c(0, 1, 0), c(J, J, p))
  
  # NA checking
  if(any(is.na(data.frame(items, z_reg)))) {
    na    <- apply(is.na(data.frame(items, z_reg)), 1, sum)
    items <- items[-which(na != 0), ]
    z_reg <- z_reg[-which(na != 0), ]
    print(paste(sum(na != 0), ' rows were removed', sep = ''))
  }
  
  # estimation
  par <- nlminb(start     = init,
                objective = irt2preglikLA,
                X         = items,
                Z         = z_reg,
                lower     = lo,
                control   = list(rel.tol = 1e-3, x.tol = 1e-3))
  
  # approximate observed information matrix
  H<-hessian(fun   = irt2preglikLA,
             param = par$par,
             X     = items,
             Z     = z_reg,
             fun0  = par$objective)
  V     <- solve(H)
  colnames(V) <- rownames(V) <- c(paste('delta', colnames(items), sep='_'),
                                  paste('alpha',colnames(items), sep = '_'),
                                  paste('beta', colnames(z_reg), sep = '_'))
  se    <- sqrt(diag(V))
  dof   <- nrow(items) - length(par$par)
  coeff <- data.frame(par$par,
                      se,
                      (par$par - rep(c(0, 1, 0),c(J, J, p)))/se,
                      2*(1 - pt(abs((par$par - rep(c(0, 1, 0), c(J, J, p)))/se), df = dof)))
  rownames(coeff) <- rownames(V)
  colnames(coeff) <- c('Estimate',
                       'Std. Error',
                       't value',
                       'Pr(|>t|)')
  delta <- coeff[seq(2*J), ]
  beta  <- coeff[-seq(2*J), ]
  
  loglik <- par$objective
  iter   <- par$iterations
  
  mod        <- list(call    = match.call(),
                     coef    = delta,
                     beta    = beta,
                     iter    = iter,
                     loglik  = -loglik,
                     vcov    = V,
                     items   = items,
                     linpred = z_reg)
  class(mod) <- append(class(mod), 'rasch')
  return(mod)
}