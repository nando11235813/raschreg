# print
print.rasch <- function(x, ...){
  stopifnot(inherits(x, 'rasch'))
  print(x$call)
  pnames  <- unlist(lapply(strsplit(rownames(x$coef), '_'),
                           function(x)x[1]))
  pvnames <- rownames(x$coef)
  if ('alpha' %in% pnames){
    those  <- which(pnames == 'alpha')
    disc   <- x$coef[ those, ]
    x$coef <- x$coef[-those, ]
    cat('\n', 'Discrimination parameters', '\n')
    alpha  <-round(disc[, 1], 3)
    attr(alpha, 'names') <- pvnames[those]
    print(alpha)
  }
  if ('gamma' %in% pnames){
    those  <- which(pnames == 'gamma')
    guess  <- x$coef[ those,]
    x$coef <- x$coef[-those,]
    cat('\n', 'Pseudo-guessing parameters', '\n')
    gamma  <- round(guess[, 1], 3)
    attr(gamma, 'names') <- pvnames[those]
    print(gamma)
  }
  cat('\n', 'Difficulty parameters', '\n')
  est <- round(x$coef[, 1], 3)
  attr(est, 'names') <- pvnames[pnames == 'delta']
  print(est)
  if('beta' %in% names(x)){
    cat('\n', 'Regression parameters', '\n')
    est<-round(x$beta[, 1], 3)
    attr(est,'names') <- rownames(x$beta)
    print(est)
  }
}

# coef
coef.rasch <- function(object, ...){
  stopifnot(inherits(object, 'rasch'))
  out     <- vector('list')
  pnames  <- unlist(lapply(strsplit(rownames(object$coef), '_'), function(x) x[1]))
  pvnames <- rownames(object$coef)
  if ('alpha'%in%pnames){
    those  <- which(pnames == 'alpha')
    est.a  <- object$coef[those, 1]
    object$coef <- object$coef[-those,]
    attr(est.a,'names') <- pvnames[those]
    out$est.a <- est.a
  }
  if ('gamma'%in%pnames){
    those  <- which(pnames == 'gamma')
    est.g  <- object$coef[those, 1]
    object$coef <- object$coef[-those, ]
    attr(est.g,'names') <- pvnames[those]
    out$est.g <- est.g
  }
  est.d <- object$coef[,1]
  attr(est.d, 'names') <- rownames(object$coef)
  out$est.d <- est.d
  if ('beta'%in%names(object)){
    est.b <- object$beta[, 1]
    attr(est.b,'names') <- rownames(object$beta)
    out$est.b <- est.b
  }
  return(out)
}

# summary
summary.rasch <- function(object, cov_type = 'hessian', correlation = FALSE, signif.stars = getOption("show.signif.stars"), ...){
  stopifnot(inherits(object, 'rasch'))
  if(! cov_type %in% c('hessian', 'opg', 'sandwich')) stop("cov_type must be one of 'wald', 'opg' or 'sandwich'")
  out <- list()
  
  cat(paste('n',      nrow(object$items),     sep='      '), '\n')
  cat(paste('logLik', round(object$loglik,3), sep=' ')     , '\n') 
  cat(paste('AIC',    round(AIC(object), 3), sep='    ')   , '\n')
  cat(paste('BIC',    round(BIC(object), 3), sep='    ')   , '\n')
  out$AIC    <- AIC(object)
  out$BIC    <- BIC(object)
  out$logLik <- object$loglik

  # coefficient estimates
  est <- object$coef
  if ('beta' %in% names(object)) est <- rbind(est, object$beta) 

  # coefficient names
  pvnames <- rownames(est)
  pnames  <- unlist(lapply(strsplit(pvnames, '_'),function(x) x[1]))
  
  # covariance matrix estiamtion (if necessary)
  if (cov_type != 'hessian'){
    V       <- vcov(object, type = cov_type)
    std_err <- sqrt(diag(V))
    est[,2] <- std_err[1:length(pnames)]
    h0      <- ifelse(pnames == 'alpha', 1, 0)
    est[,3] <- (est[,1] - h0)/est[,2]
    dof     <- nrow(object$items) - length(pnames)
    est[,4] <- 2*(1 - pt(abs(est[,3]),df = dof))
  }

  if ('alpha' %in% pnames){
    cat('\n')
    cat('Discrimination parameters','\n')
    a  <- round(est[which(pnames == 'alpha'),],3)
    if(any(a[, 4] <= 0.001, na.rm = TRUE)) a[a[, 4] == 0,4]<-'<0.001'
    ss <- sstars(a[,4])
    names(a) <- c('Estimate',
                  'Std. Error',
                  't value',
                  'Pr(>|t|)')
    if (signif.stars) {
      a <- data.frame(a, ss, check.names = FALSE)
      names(a)[ncol(a)] <- ''
    }
    print(a)
    out$alpha   <- a
  }
  cat('\n')
  cat('Difficulty parameters','\n')
  d  <- round(est[which(pnames == 'delta'),],3)
  if(any(d[,4] <= 0.001, na.rm = TRUE)) d[which(d[,4] <= 0.001), 4] <- '<0.001'
  ss <- sstars(d[, 4])
  names(d) <- c('Estimate',
                'Std. Error',
                't value',
                'Pr(>|t|)')
  if (signif.stars) {
    d <- data.frame(d, ss, check.names=FALSE)
    names(d)[ncol(d)] <- ''
  }
  print(d)
  cat('___', '\n')
  print("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1")
  out$difficulty <- d
  
  if ('beta'%in%names(object)){
    cat('___', '\n')
    cat('Regression parameters', '\n')
    b <- round(est[which(pnames == 'beta'),],3)
    if(any(b[, 4] <= 0.001, na.rm = TRUE)) b[b[, 4] == 0,4] <- '<0.001'
    ss <- sstars(b[, 4])
    names(b)<-c('Estimate',
                 'Std. Error',
                 't value',
                 'Pr(>|t|)')
    if (signif.stars) {
      b <- data.frame(b, ss, check.names = FALSE)
      names(b)[ncol(b)] <- ''
    }
    print(b)
    cat('___','\n')
    print("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1")
    out$beta <- b
  }
  if (correlation == TRUE){
    corr <- cov2cor(vcov(object, type = cov_type))
    cat('___', '\n')
    print(corr, 2)
    out$correlation <- corr
  }
  invisible(out)
}

# vcov
vcov.rasch <- function(object, type = 'hessian', ...){
  stopifnot(inherits(object, 'rasch'))
  V <- switch(type,
              'hessian'  = hess(object),
              'opg'      = opg(object),
              'sandwich' = sandwich(object))
  return(V)
}

# confint
confint.rasch <- function(object, parm, level = 0.95, type = 'wald', B = 99, ...){
  stopifnot(inherits(object, 'rasch'))
  ocf    <- object$coef
  cf     <- ocf[, 1]
  dnames <- rownames(ocf)
  a      <- (1 - level)/2
  
  if (type == 'wald'){
    a      <- c(a, 1 - a)
    dof    <- nrow(object$items) - length(unlist(coef(object)))
    tval   <- qt(a, dof)
    se     <- ocf[,2]
    ci     <- cf + se %o% tval
    rownames(ci) <- dnames
    colnames(ci) <- paste(round(a*100, 1), '%')
    if ('beta' %in% names(object)){
      cf.b   <- object$beta[, 1]
      bnames <- rownames(object$beta)
      se.b   <- object$beta[, 2]
      ci.b   <- cf.b + se.b %o% tval
      rownames(ci.b) <- bnames
      ci     <- rbind(ci,ci.b)
    }
  }
  if (type == 'profile'){
    ci <- proflik(object, alpha = 1 - level)
  }
  if (type == 'boot'){
    boots <- pbootr(object, B = B)
    reps  <- lapply(boots, function(x) x$coef[, 1])
    reps  <- matrix(unlist(reps), nrow = B, byrow = TRUE)
    ci    <- t(apply(reps, 2, quantile, probs = c(a, 1-a)))
    rownames(ci) <- rownames(object$coef)
    colnames(ci) <- paste(round(c(a, 1 - a)*100, 1), '%')
  }
  if(!missing(parm)){
    rows <- which(rownames(ci) %in% parm)
  } else rows <- 1:nrow(ci)
  
  return(ci[rows, ])
}

# update
update.rasch <- function(object, formula., ..., evaluate = TRUE) {
  stopifnot(inherits(object, 'rasch'))
  call   <- object$call
  f_reg  <- formula(paste('~', paste(colnames(object$linpred), collapse = '+'),sep = ''))
  extras <- match.call(expand.dots = FALSE)$...
  if (is.null(call)) 
    stop("Need an object with a 'call' component")
  if (!missing(formula.))
    call$f_reg <- update.formula(f_reg, formula.)
  if(length(extras)>=1) {
	  existing <- !is.na(match(names(extras), names(call)))
	  ## do these individually to allow NULL to remove entries.
	  for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
	  if(any(!existing)) {
	    call <- c(as.list(call), extras[!existing])
	    call <- as.call(call)
	  }
  }
  if (evaluate) 
    eval(call, parent.frame())
  else call
}

# loglik
logLik.rasch <- function(object, ...){
  stopifnot(inherits(object, 'rasch'))
  return(object$loglik)
}

# AIC
AIC.rasch <- function(object, ..., k = 2){
  stopifnot(inherits(object, 'rasch'))
  if(!missing(...)){
    models  <- list(object, ...)
    # log-likelihood
    ll      <- as.numeric(lapply(models, logLik))
    # number of parameters
    num_par <- function(mod) nrow(mod$coef)
    npar    <- sapply(models, num_par)
    # output
    aic     <- data.frame(df = npar, AIC = -2*ll + k*npar)
    Call    <- match.call()
    rownames(aic) <- as.character(Call[-1])
    return(aic)
  } else {
    return(k*nrow(object$coef) - 2*object$loglik)
  }
}

# BIC
BIC.rasch <-function(object, ...){
  stopifnot(inherits(object, 'rasch'))
  if(!missing(...)){
    models  <- list(object, ...)
    # log-likelihoods
    ll      <- as.numeric(lapply(models,logLik))
    # numer of parameters
    num_par <- function(mod) nrow(mod$coef)
    npar    <- sapply(models, num_par)
    # number of observations
    nobs    <- function(mod) nrow(mod$items)
    n       <- sapply(models, nobs)
    # output
    bic     <- data.frame(df = npar,BIC = -2*ll + log(n)*npar)
    Call    <-match.call()
    rownames(bic) <- as.character(Call[-1])
    return(bic)
  } else {
    return(log(nrow(object$items))*nrow(object$coef) - 2*object$loglik)
  }
}

# hessian
hessian<-function(fun, param, ..., fun0 = NULL){
  
  h <- (.Machine$double.eps)^(1/3)
  p <- length(param)
  e <- diag(h*(1 + abs(param)), p)
  
  if (is.null(fun0)) fun0 <- fun(param, ...)
  deriva2 <- function(i, j, ...) (fun(param + e[, i] + e[, j],...) -
                                    get(paste('fun', i, sep = '')) -
                                    get(paste('fun', j, sep = '')) + fun0)/(e[i, i]*e[j, j])
  hess    <- matrix(0, p, p)
  for (i in 1:p) {
    assign(paste('fun', i, sep = ''),fun(param + e[, i], ...))
    hess[i, i] <- deriva2(i, i, ...)
    if (i<p) {
      for (j in (i + 1):p) {
        assign(paste('fun', j, sep = ''), fun(param + e[, j], ...))
        hess[i, j] <- hess[j, i] <- deriva2(i, j, ...)
      }
    }
  }
  return(hess)
}

# gradient
gradient <- function(fun, param, ..., fun0 = NULL){
  
  h <- sqrt(.Machine$double.eps)
  p <- length(param)
  e <- diag(h*(1 + abs(param)), p)
  
  if (is.null(fun0)) fun0 <- fun(param, ...)
  deriva1 <- function(i, ...) (fun(param + e[, i], ...) - fun0)/(e[i, i])
  grad    <- matrix(0, 1, p)
  for (i in 1:p) {
    assign(paste('fun', i, sep = ''),fun(param + e[, i], ...))
    grad[1, i] <- deriva1(i, ...)
  }
  return(grad)
}

# ability
ability <- function(mod, type = 'lord'){
 	stopifnot(inherits(mod, 'rasch'))
  pnames  <- unlist(lapply(strsplit(rownames(mod$coef),'_'), function(x)x[1]))
  pvnames <- rownames(mod$coef)
  J <- ncol(mod$items)
	if ('alpha' %in% pnames){
	  alpha    <- mod$coef[which(pnames == 'alpha'), 1]
	  mod$coef <- mod$coef[-which(pnames == 'alpha'),]
	  if(length(alpha)== 1) alpha <- rep(alpha, J)
	} else alpha <- rep(1, J)
	if ('gamma' %in% pnames){
	  pg       <- mod$coef[which(pnames == 'gamma'), 1]
	  mod$coef <- mod$coef[-which(pnames == 'gamma'), ]
	} else pg  <-rep(0, J)
  delta <- mod$coef[, 1]
	X     <- mod$items
	Z     <- mod$linpred
	# b max
	if ('beta' %in% names(mod)){
	  J    <- length(delta)
	  beta <- mod$beta[, 1]
	  bmax <- apply(data.frame(X, Z), 1,
	                function(rowi)optimize(huc,
	                                       interval = c(-4, 4),
	                                       x = rowi[seq(J)],
	                                       d = delta,
	                                       a = alpha,
	                                       beta = beta,
	                                       z = rowi[-seq(J)],
	                                       tol = 1e-4 ,
	                                       maximum=TRUE)$maximum)
	} else {
	  bmax <- apply(X, 1, function(rowi)optimize(hbc,
	                                         interval = c(-4, 4),
	                                         x = rowi,
	                                         a = alpha,
	                                         d = delta,
	                                         tol = 1e-4,
	                                         maximum = TRUE)$maximum)
	  }
	abty <- data.frame(theta = bmax)
	if (type=='lord'){
	  abty$bias   <- bias(mod, theta = abty$theta)
	  abty$thetac <- abty$theta + abty$bias
	}
	return(abty)
}

# bias
bias <- function(mod, theta){
  abs   <- info(mod, theta = theta, plot = FALSE)
  k     <- length(theta)
  J     <- ncol(abs) - 2
  b     <- rep(0, k)
  coefs <- coef(mod)
  delta <- coefs$est.d
  alpha <- coefs$est.a
  if (is.null(alpha)) alpha <- rep(1, J) else {
    if(length(alpha) == 1) alpha <- rep(alpha, J)
  }
  
  for (i in 1:k){
    probs <- 1/(1+exp(-(alpha*(abs[i, 'theta'] - delta))))
    b[i]  <- sum(alpha*abs[i, 2:(J + 1)]*(probs - 0.5))/(abs[i, 'Total']^2)
  }
  return(b)
}

# restricted cubic splines
rcs<-function(x, m = NULL, knots = NULL, scale = TRUE){
	if(is.null(knots)) {
		fr    <- 1/(m +1)
		knots <- quantile(x, probs = seq(fr, 1 - fr, fr))
	}
	if(is.null(m)) m <- length(knots)
	Cs  <- matrix(NA,
	              nrow = length(x),
	              ncol = m - 2)
	for (i in 1:(m-2)) Cs[,i] <- apply(cbind(0, (x - knots[i])^3), 1, max) - apply(cbind(0, (x - knots[m - 1])^3), 1, max) * (knots[m] - knots[i])/(knots[m] - knots[m - 1]) + apply(cbind(0,(x - knots[m])^3), 1, max)*(knots[m - 1] - knots[i])/(knots[m] - knots[m - 1])
	Cs  <- cbind(x, Cs)
	colnames(Cs) <-paste('C', 0:(m-2), sep = '')
	if(scale) {
		sds <- apply(Cs, 2, sd)
		Cs  <- scale(Cs, center = FALSE, scale = sds)
	}
	attr(Cs,'class')         <- c('rcs', 'basis', 'matrix')
	attr(Cs,'intercept')     <- FALSE
	attr(Cs,'Boundary.knots')<- range(x)
	attr(Cs,'knots')         <- knots
	return(Cs)
}

# predict restricted cubic splines
predict.rcs<-function(object, new_x, ...){
	knots   <- attr(object, 'knots')
	b_knots <- attr(object, 'Boundary.knots')
	m       <- length(knots)
	sds     <- attr(object, 'scale')

	pCs <- matrix(NA,
	              nrow = length(new_x),
	              ncol = m - 2)
	for (i in 1:(m-2)) pCs[, i] <- apply(cbind(0, (new_x - knots[i])^3), 1, max) - apply(cbind(0, (new_x - knots[m - 1])^3), 1, max)*(knots[m] - knots[i])/(knots[m] - knots[m - 1]) + apply(cbind(0, (new_x - knots[m])^3), 1, max)*(knots[m - 1] - knots[i])/(knots[m] - knots[m - 1])
	pCs <- cbind(new_x,pCs)
	colnames(pCs) <- colnames(object)
	if(!is.null(sds)) {
		pCs <- scale(pCs, center = FALSE, scale = sds)
	}
	attr(pCs,'class')          <- c('rcs', 'object', 'matrix')
	attr(pCs,'intercept')      <- FALSE
	attr(pCs,'Boundary.knots') <- range(new_x)
	attr(pCs,'knots')          <- knots

	return(pCs)
}

# anova
anova.rasch<-function(object, ..., ref = 1){
  models  <- list(object, ...)
  israsch <- lapply(models,
                    function(x) inherits(x,'rasch'))
  israsch <- unlist(israsch)
  if (any(!israsch)) stop("Some of the models are not 'rasch' objects")
  
  coefs <- lapply(models,
                  function(x) rbind(x$coef,x$beta))
  # number of free parameters
  nfp   <- function(x) sum(!is.na(x))
  npars <- unlist(lapply(coefs,
                         function(x) nfp(x[,2])))
  lliks <- unlist(lapply(models,
                         function(x) logLik(x)))
  # Likelihood ratio test
  LRT  <- c(NA, 2*(lliks[-ref] - lliks[ref]))
  df   <- c(NA,  npars[-ref] - npars[ref])
  if (any(df[-1]<0)) warning('Check that the reference model is the more restricted')
  pv   <- c(1 - pchisq(LRT, df))
  if (any(pv<0.001)) pv[pv<0.001] <- '<0.001'
  test <- data.frame(logLik  = lliks,
                     AIC     = AIC(object, ...)[, 2],
                     BIC     = BIC(object, ...)[, 2],
                     LRT     = LRT,
                     df      = df,
                     p.value = pv)
  alcall  <- as.list(match.call(expand.dots=TRUE))
  rn      <- as.character(lapply(alcall,function(x)x))[-1] 
  rownames(test) <- rn
  test1   <- format(test, digits = 5)
  test1[is.na(test)] <- ''
  
  cat('Likelihood Ratio Test', '\n')
  print(test1)

  invisible(test)
}

# parametric bootstrap
pbootr <- function(mod, B = 99, trace = TRUE){
  J    <- ncol(mod$items)
  n    <- nrow(mod$items)
  call <- mod$call
  
  # parameter extraction
  delta <- coef(mod)$est.d
  alpha <- coef(mod)$est.a
  if (is.null(alpha)) alpha<-rep(1, J)
  if (length(alpha) == 1 ) alpha <- rep(alpha, J)
  if ('beta' %in% names(mod)){
    reg  <- mod$linpred
    beta <- coef(mod)$est.b
  } else {
    reg  <- NULL
    beta <- NULL
  }
  
  # list for models
  mods <- vector('list',B)
  
  for (i in 1:B){
    X_i        <- sim_rasch(n, J, delta = delta, alpha = alpha, reg = reg, beta = beta)
    if (trace) cat('Fitting model ',i,'\n')
    call$items <- as.name('X_i')
    mods[[i]]  <- eval(call)
  }
  names(mods)  <- paste('mod', 1:B, sep='_')
  invisible(mods)
}

# profile likelihood CIs
proflik <- function(mod, alpha = 0.05){
  est  <- coef(mod)
  init <- unlist(est$est.d)
  if('est.a' %in% names(est)) init <- c(init, log(est$est.a))
  if('est.b' %in% names(est)) {
    init <- c(init, est$est.b)
    Z    <- mod$linpred
  } else Z <- NULL
  qch2 <- qchisq(1 - alpha, 1)/2
  L0   <- mod$loglik - qch2
  
  items <- mod$items
  npar  <- length(init)
  CI    <- matrix(0, ncol = 2, nrow = npar)
  
  #log-likelihood function
  flik <- as.character(mod$call[[1]])
  flik <- paste(flik, 'likLA', sep = '')

  fzero <- function(h){
    initL   <- init
    fixL    <- rep(NA, npar)
    fixL[i] <- initL[i] <- initL[i] + h
    if(is.null(Z)) prof <- nlminb(start = initL, objective = get(flik), X = items, control = list(rel.tol = 1e-7, x.tol = 1e-7), fixed = fixL)
    else           prof <- nlminb(start = initL, objective = get(flik), X = items, Z = Z, control = list(rel.tol = 1e-7, x.tol = 1e-7), fixed = fixL)               
    -prof$objective - L0
  }
  
  # free parameters
  free <- 1:npar
  if (any(is.na(mod$coef[,2]))){
    free <- free[-which(is.na(mod$coef[,2]))]
  }

  cat('Profiling the likelihood ...','\n')
  for (i in free){
    # extremes of intervals 
    xl <- -2 ; fxl <- fzero(-2)
    xr <-  2 ; fxr <- fzero(2)
    while(fxl > 0){
      xl <- xl*2
      fxl <- fzero(xl)
    }
    while(fxr > 0){
      xr <- xr*2
      fxr <- fzero(xr)
    }
    
    CI[i,1] <- init[i] + uniroot(fzero, interval = c(xl, 0), f.lower = fxl, f.upper = qch2, tol = 1e-3)$root
    CI[i,2] <- init[i] + uniroot(fzero, interval = c(0, xr), f.lower = qch2, f.upper = fxr, tol = 1e-3)$root
    print(names(init)[i])
  }
  # exponentiate discrimination parameters
  if ('est.a' %in% names(est)){
    k <- length(est$est.a)
    J <- ncol(items)
    CI[(J + 1):(J + k),] <- exp(CI[(J + 1):(J + k),])
  }
  
  rownames(CI) <- rownames(mod$coef)
  colnames(CI) <- paste(round(c(alpha/2, 1 - alpha/2)*100, 1), '%',sep = '')
  return(CI)
}

# test simple restrictions (Id*par_vec = par_value)
test <- function(mod, restr){
  param <- mod$coef[, 1]
  rn    <- rownames(mod$coef)
  if('beta' %in% names(mod)) {
    param <- c(param, mod$beta[,1])
    rn    <- c(rn, rownames(mod$beta))
  }
  npar <- length(param)

  # checking restrictions vector
  if (length(restr) != npar) stop("Wrong length in 'restr'")

  # refit the model s.t. restrictions
  modr   <- update(mod, formula. = f, fixed = restr)
  paramr <- modr$coef[, 1]
  if ('beta' %in% names(modr)) paramr <- c(paramr, mod$beta[, 1])

  # log-likelihood function
  flik <- as.character(mod$call[[1]])
  flik <- paste(flik, 'likLA', sep = '')

  # score vector
  score <- gradient(fun   = get(flik),
                    param = paramr,
	                  X     = mod$items,
	                  fixed = restr*NA)
  hess  <-  hessian(fun   = get(flik),
                    param = paramr,
	                  X     = mod$items,
	                  fixed = restr*NA)
  # degrees of freedom
  dof <- sum(!is.na(restr))

  # Rao test statistic
  paramr <- modr$coef[, 1]
  if('beta' %in% names(modr)) param <- c(param, mod$beta[, 1])
  LM     <- score%*%qr.solve(hess, t(score))
  pv_sc  <- 1 - pchisq(LM, dof)

  # Likelihood Ratio test statistic
  LR     <- -2*(modr$loglik - mod$loglik)
  pv_lr  <- 1 - pchisq(LR, dof)

  # Wald test statistic
  those <- which(!is.na(restr))
  h     <- (param - restr)[those]
  V     <- mod$vcov[those, those]
  W     <- h%*%qr.solve(V, t(h))
  pv_w  <- 1 - pchisq(W, dof)

  test  <- data.frame(stat = c(LM, LR, W),
                      dof  = rep(dof, 3),
                      pv   = c(pv_sc, pv_lr, pv_w))
  rownames(test) <- c("Rao's Score",
                      'Likelihood Ratio',
                      'Wald')

  rn <- rn[those]
  for (i in 1:length(those)) {
    r <- paste(rn, restr[those][i], sep=' = ')
    cat(r, '\n')
  }
  print(format(test, digits = 4))
  return(test)
}

# signification stars
sstars <- function(pv){
  ss <- ifelse(pv > 0.1, '',
               ifelse(pv > 0.05, '.',
                      ifelse(pv > 0.01, '*',
                             ifelse(pv > 0.001, '**','***'))))
  if(any(is.na(ss))) ss[which(is.na(ss))]<-''
  return(ss)
}

# cross product gradient covariance estimation
opg <- function(mod){
  # log-likelihood function
  flik  <- as.character(mod$call[[1]])
  flik  <- paste(flik, 'likLA', sep = '')
  
  # model parameters
  cmod  <- coef(mod)
  pars  <- cmod$est.d
  if ('est.a' %in% names(cmod)) pars <- c(pars, log(cmod$est.a))
  if ('est.b' %in% names(cmod)) {
    pars <- c(pars, cmod$est.b)
    Z    <- mod$linpred
  } else Z <- NULL
  X     <- mod$items
  n     <- nrow(X)
  J     <- ncol(X)
  grads <- matrix(NA, nrow = n, ncol = length(pars))

  if(is.null(Z)) for (i in 1:n) grads[i, ] <- gradient(fun = get(flik), param = pars, X = X[i,,drop=FALSE])
  else           for (i in 1:n) grads[i, ] <- gradient(fun = get(flik), param = pars, X = X[i,,drop=FALSE], Z = Z[i,,drop=FALSE])

  Vpars <- solve(t(grads) %*% grads)
  
  # delta method covariances (back-transform log(discrimination))
  if('est.a' %in% names(cmod)){
    h      <- rep(1, length(pars))
  	a_p    <- seq(from = J + 1, length = length(cmod$est.a))
   	h[a_p] <- exp(pars[a_p])
  	Vpars  <- outer(h, h)*Vpars
  }
	
  colnames(Vpars) <- rownames(Vpars) <- names(pars)
  return(Vpars)
}

# sandwich covariance matrix estimation
sandwich <- function(mod){
  ihess    <- solve(hess(mod)) # extracts hessian^{-1}
  c_opg    <- opg(mod)  # extracts outer_product_gradients^{-1}
  c_sand   <- ihess %*% c_opg %*% ihess
  return(solve(c_sand))
}

# hessian covariance matrix estimation
hess <- function(mod){
  # log-likelihood function
  flik  <- as.character(mod$call[[1]])
  flik  <- paste(flik, 'likLA', sep = '')

  # model parameters
  cmod  <- coef(mod)
  pars  <- cmod$est.d
  if ('est.a' %in% names(cmod)) pars <- c(pars, log(cmod$est.a))
  if ('est.b' %in% names(cmod)) {
    pars <- c(pars, cmod$est.b)
    Z    <- mod$linpred
  } else Z <- NULL
  X     <- mod$items
  n     <- nrow(X)
  J     <- ncol(X)
  if (!is.null(Z)) {
    Vpars <- hessian(fun = get(flik), param = pars, X = X, Z = Z, fun0 = mod$loglik, fixed = NULL)
  } else {
    Vpars <- hessian(fun = get(flik), param = pars, X = X, fun0 = mod$loglik, fixed = NULL)
  }  
  Vpars <- solve(Vpars)
  # delta method covariances (back-transform log(discrimination))
  if('est.a' %in% names(cmod)){
    h      <- rep(1, length(pars))
  	a_p    <- seq(from = J + 1, length = length(cmod$est.a))
   	h[a_p] <- exp(pars[a_p])
  	Vpars  <- outer(h, h)*Vpars
  }

  colnames(Vpars) <- rownames(Vpars) <- names(pars)
  return(Vpars)
}

# prepare explanatory variables 
pev <- function(z, f){
  # variable clases
  cl <- unlist(lapply(z,class))
  
  # center numeric and integer variables
  if (any(cl %in% c('numeric', 'integer'))){
    numint <- which(cl %in% c('numeric', 'integer'))
    k1     <- length(numint)
    for (j in 1:k1) z[,numint[j]] <- scale(z[,numint[j]], scale = FALSE)
  }
  # character variables as factors
  if (any(cl == 'character')){
    char <- which(cl == 'character')
    k2   <- length(char)
    for (j in 1:k2) z[,char[j]] <- as.factor(z[,char[j]])
  }
  # factor variables coding
  if (any(cl %in% c('character','factor'))){
    fctr <- which(cl %in% c('character','factor'))
    k3   <- length(fctr)
    for (j in 1:k3) {
      n_cat <- length(levels(z[,fctr[j]]))
      contrasts(z[,fctr[j]]) <- contr.sum(n_cat)
    }
  }
  
  # extract variables from formula
  mm    <- model.matrix.lm(f, z, na.action = 'na.pass')
  oldnm <- colnames(mm)
  
  # extracts factor names
  ctr <- attr(mm,'contrasts')
  
  # redocify factors (if any)
  if (length(ctr)>0) {
  	fctr_nm <- names(ctr)
  	# recode 'name(category)'
  	ncat <- names(ctr)
  	k <- length(ncat)
  	for (i in 1:2){
  		ncol    <- grep(ncat[i],colnames(mm))
  		if (length(ncol)>1) ncol <- ncol[1]
  		catnum  <- gsub(ncat[i],'',colnames(mm)[ncol])
  		catname <- rownames(ctr[[i]])[which(ctr[[i]]=='1')]
  		# finally
  		colnames(mm)[ncol]<- paste(ncat[i],'(',catname,')',sep='')
  	}
  }
  # check for interactions
  ints <- grepl(':',colnames(mm))
  if (any(ints)){
  	ints_i <- which(ints)
  	for (i in ints_i){
  		splt <- unlist(strsplit(colnames(mm)[i],':'))
  		# substitution
  		splt   <- sapply(splt, function(x)colnames(mm)[oldnm==x])
  		# recode 'name(category):name(category)'
  		colnames(mm)[i] <- paste(splt, collapse=':')
  	}
  }
  # remove intercept
  if('(Intercept)' %in% colnames(mm)) {
    icpt <- which(colnames(mm) == '(Intercept)')
    mm <- mm[,-icpt,drop=FALSE]
  }
  return(mm)
}
