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
    a      <- round(est[which(pnames == 'alpha'),],3)
    a[, 4] <- pvtol(a[, 4], 0.001)
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
  d[, 4] <- pvtol(d[, 4], 0.001)
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
    b[, 4] <- pvtol(b[, 4], 0.001)
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
    ci <- proflik(object, level = 1 - level)
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
  call   <- getCall(object)
  if ('linpred' %in% names(object)) {
    f_reg   <- eval(call[[3]])
  } else f_reg <- NULL
  extras <- match.call(expand.dots = FALSE)$...
  if (is.null(call)) 
    stop("Need an object with a 'call' component")
  if (!missing(formula.)){
    f_upd <- update.formula(f_reg, formula.)
    call  <- call_modify(call, f_reg = f_upd)
  }
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
  test <- data.frame(logLik  = lliks,
                     AIC     = AIC(object, ...)[, 2],
                     BIC     = BIC(object, ...)[, 2],
                     LRT     = LRT,
                     df      = df,
                     p.value = pvtol(pv, 0.001))
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
    X_i        <- sim_rasch(n, delta = delta, alpha = alpha, reg = reg, beta = beta)
    if (trace) cat('Fitting model ',i,'\n')
    call$items <- as.name('X_i')
    mods[[i]]  <- eval(call)
  }
  names(mods)  <- paste('mod', 1:B, sep='_')
  invisible(mods)
}

# profile likelihood CIs
proflik <- function(mod, level = 0.05){
  est  <- coef(mod)
  init <- unlist(est$est.d)
  if('est.a' %in% names(est)) init <- c(init, log(est$est.a))
  if('est.b' %in% names(est)) {
    init <- c(init, est$est.b)
    Z    <- mod$linpred
  } else Z <- NULL
  qch2 <- qchisq(1 - level, 1)/2
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
  colnames(CI) <- paste(round(c(level/2, 1 - level/2)*100, 1), '%',sep = '')
  return(CI)
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
    Vpars <- hessian(fun = get(flik), param = pars, X = X, Z = Z, fun0 = -mod$loglik, fixed = NULL)
  } else {
    Vpars <- hessian(fun = get(flik), param = pars, X = X, fun0 = -mod$loglik, fixed = NULL)
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
  	for (i in 1:k){
  		ncol    <- grep(ncat[i],colnames(mm))
  		catnum  <- gsub(ncat[i], '', colnames(mm)[ncol])
  		catname <- names(which(apply(ctr[[i]]==1,1,sum)==1))
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

# tests linear constraints of the form 'R*pars = b'
lincon <- function(mod, R, b, cov_type = 'hessian'){
  if(class(b) == 'numeric') b <- matrix(b, ncol=1)
  # extract model parameters
  est <- mod$coef
  if ('beta' %in% names(mod)) est <- rbind(est, mod$beta) 

  # coefficient names
  pvnames <- rownames(est)
  est <- est[,1]
  
  # covariance matrix
  V <- vcov(mod, type = cov_type)
  
  # check R, b dimensions
  if (! 'matrix' %in% class(R)) stop('R must be an Q*p matrix')
  if (ncol(R) != length(est)) stop('Wrong number of columns in R')
  if (nrow(R) != nrow(b)) stop('R and/or b incorrectly specified')
  
  # Wald statistic
  Rpb <- R%*%est - b
  W   <- t(Rpb) %*% solve(R %*% V %*%t(R), Rpb)
  
  # degrees of freedom
  n  <- nrow(mod$items)
  p  <- length(est)
  Q  <- nrow(R)
  out <-data.frame(statistic = W,
                   dfn = Q,
                   dfd = n - p,
                   pv = 1 - pf(W, Q, n - p))
  colnames(out) <- c('W statistic','df','res.df','Pr(>F)')
  # q imprima el nombre del test y los contrastes de R y b
  cat('\n')
  cat('Wald statistic for constraints R*par_vec = b','\n')

  f1 <- function(x){
    if(x == 1){
      return('+')
    } else {
      if(x == -1) {
        return('-')
      } else return(as.character(x))
    }
  }
  cat('\n')
  cat('Hypothesis:','\n')
  for (i in 1:nrow(R)){
    ri    <- R[i,]
    those <- pvnames[which(ri != 0)]
    coefs <- sapply(ri[ri != 0], f1)
    eq    <- paste(paste(paste(coefs, those, sep = ''), collapse = ''),b[i],sep=' = ')
    cat(eq, '\n')
  }
  cat('\n')
  print(out)
  invisible(out)
}

# enfore linear constraints on parameter vector
constrainer <- function(pars, R, b){
	first <-apply(R,1,function(x)min(which(x!=0)))
	
	# set a 'one'
	ind <- cbind(seq(nrow(R)),first)
	R1  <- diag(1/R[ind]) %*% R
	b1  <- diag(1/R[ind]) %*% b
	
	# por ultimo habria que obligar a 'pars' a cumplir las restricciones
	for (i in 1:nrow(R1)){
		ri1 <- ri <- R1[i,]
		bi  <- b1[i,]
		ri1[first[i]]  <- 0
		pars[first[i]] <- bi - sum(ri1*pars) 
	}
	return(pars)
}

# drop all possible single terms to a model
drop1.rasch <- function(object, scope = NULL, test = 'LRT', cov_type = 'hessian', scale = 0, k = 2, ...){
  stopifnot(inherits(object, 'rasch'))
  if (!'beta' %in% names(object)) stop("'object' must have explanatory variables")
  # extract model formula
  if (is.null(scope)) {
    f <- eval(object$call[[3]])
  } else {
    f <- scope
  }
  xj <- all.vars(f)
  if (test == 'LRT'){
    mods <- vector('list')
    for (j in 1:length(xj)){
      fj        <- as.formula(paste('~.-', xj[j], ':.', sep = ''))
      mods[[j]] <- update(object, formula. = fj)
    }
    # extraction
    ll   <- lapply(mods, function(x) x$loglik)
    npar <- lapply(mods, function(x) ncol(x$linpred))
    aic  <- lapply(mods, function(x) AIC(x))
    bic  <- lapply(mods, function(x) BIC(x))
    
    # full model
    npar0 <- ncol(object$linpred)
    ll0   <- object$loglik
    
    out <- data.frame(df = npar0 - unlist(npar),
                      AIC  = unlist(aic),
                      BIC  = unlist(bic),
                      LRT  = -2*(unlist(ll) - ll0))
    out$'Pr(>Chi)' <- pvtol(1 - pchisq(out$LRT, out$df), 0.001)
    rownames(out) <- xj
  } else {
    # extract model matrix names
    mm <- colnames(object$linpred)
    r  <- matrix(0, ncol = length(mm)) 
    # number of irt parameters
    np <- nrow(object$coef)
    # covariance matrix (regression effects)
    V  <- vcov(object, type = cov_type)
    V1 <- solve(V[-seq(np),-seq(np)])
    # regression parameters
    beta <- object$beta[,1]
    Q   <- length(xj)
    wk  <- pk <- dfn <- rep(0, Q)
    dfd <- nrow(object$items) - (np + length(mm))
    for (k in 1:Q) {
      r[1, grep(xj[k], mm)] <- 1
      dfn[k] <- sum(r)
      rb     <- r%*%beta
      wk[k]  <- t(rb) %*% (r %*% V1 %*% t(r)) %*% rb
      pk[k]  <- 1 - pf(wk[k], 1, dfd)
      # reset r
      r  <- matrix(0, ncol = length(mm))
    }
    out <- data.frame(dfn = as.integer(dfn),
                      dfd = as.integer(dfd),
                      W   = wk)
    out$'Pr(>F)'  <- pvtol(pk, 0.001)
    rownames(out) <- xj
  }
  invisible(out)
}

# Cook's distance
cooksD <- function(mod, cov_type = 'hessian', trace = TRUE){
  stopifnot(inherits(mod, 'rasch'))
  # extract parametes
  b <- mod$coef[,1]
  if ('beta' %in% names(mod)) {
    b  <- c(b, mod$beta[,1])
    Z  <- mod$linpred
    pz <- ncol(Z)
  } else {
    Z  <- NULL
    pz <- 0
  }
  # extract covariance matrix
  Vb <- vcov(mod, type = cov_type)
  # extract items
  its <- mod$items
  n   <- nrow(its)
  p   <- ncol(its) + pz
  cd  <- rep(0, n)

  # calculate distances
  for (i in 1:n){
    if (is.null(Z)){
      mod_i  <- update(mod, items = its[-i, ], init = b)
    } else {
      mod_i  <- update(mod, items = its[-i, ], z_reg = Z[-i, ], init = b)
    }
    b_i <- mod_i$coef[,1]
    if ('beta' %in% names(mod_i)) b_i <- c(b_i, mod_i$beta[,1])
    cd[i] <- t(b - b_i)%*%qr.solve(Vb, b - b_i)/p
    if(trace == TRUE) cat(paste('deleting observation:',i, sep=' '),'\n')
  }
  id <- seq(n)
  D <- ggplot(data.frame(id, cd),
             aes(x = id, y = cd)) +
        geom_point() +
        geom_segment(x = id, xend = id, y = 0, yend = cd) +
        xlab('id') + ylab("Cook's distance") +
        geom_hline(yintercept = 4/n, color = 'tomato') +
        geom_text(aes(label = ifelse(cd > 4/n, id,'')),
                  hjust = 0, vjust = 0) +
        ylim(0,max(cd))
  print(D)
  invisible(cd)  
}

# formting small p-values
pvtol <- function(pv, tol = 0.01, digits = 3){
  pv <- round(pv, digits)
  if(any(pv <= tol, na.rm = TRUE)) {
    pv[pv < tol] <- paste('<', tol, sep = '')
  }
  return(pv)
}

# backward variable selection
backward <- function(mod, level = 0.05, test = 'LRT', cov_type = 'hessian'){
  stopifnot(inherits(mod, 'rasch'))
  fmod  <- eval(mod$call[[3]])
  xj    <- all.vars(fmod)
  k     <- length(xj)
  
  for (j in 1:k){
    cat('----------------', '\n')
    cat(paste('step', j, sep = ':'), '\n')
    cat('----------------', '\n')
    d_i <- drop1(object   = mod,
                 test     = test,
                 cov_type = cov_type)
    pv  <- 1 - ifelse (test == rep('LRT', nrow(d_i)),
                       pchisq(d_i$LRT, d_i$df),
                       pf(d_i$W, d_i$dfn, d_i$dfd))
    if (all(pv < level)) break else {
      pvmax <- which.max(pv)
      x_out <- rownames(d_i)[pvmax]
      f_out <- as.formula(paste('~.-', x_out))
      fmod  <- update(fmod, f_out)
      mod   <- eval(call_modify(mod$call, f_reg = fmod),
                    parent.frame())
      text1 <- paste('Variable exiting the model : ', x_out, sep = '')
      text2 <- paste(text1,' (p-value: ',pvtol(pv[pvmax], 0.001),')', sep = '')
      cat(text2, '\n')
    }
  }
  invisible(mod)
}

# P(Y=y|theta)
Pj <- function(theta, a_j, d_j){
  eta_j <- a_j*(theta - d_j)
  exp(eta_j)/(1 + exp(eta_j))
}
  
# EAP
eap_ab <- function(mod, R = 100){
  cmod <- coef(mod)
  dif  <- cmod$est.d
  J    <- length(dif)
  alpha <- cmod$est.a
  if (is.null(alpha)) alpha <- rep(1, J) else {
    if(length(alpha) == 1) alpha <- rep(alpha, J)
  }

  PYy <- function(theta, y, d, a = NULL){
    J<-length(d)
    if (is.null(a)) a<-rep(1, J)
    eta_j <- a*(theta - d)
    exp(sum(y*eta_j) - sum(log(1 + exp(eta_j))))
  }

  X <- mod$items
  n <- nrow(X)
  
  if ('beta' %in% names(mod)){
    mu <- mod$linpred%*%mod$beta[,1]
  } else mu  <- rep(0, n)

  abs <- rnorm(R, mean = mu)
  ab_hat <- rep(NA, n)
  for (i in 1:n){
    den <- mean(sapply(abs, function(tita)PYy(tita,X[i,],dif,alpha)))
    num <- mean(sapply(abs, function(tita)tita*PYy(tita,X[i,],dif,alpha)))
    ab_hat[i] <- num/den
  }
  return(ab_hat)
}

# MLE
mle_ab <- function(mod){
  cmod <- coef(mod)
  dif  <- cmod$est.d
  J    <- length(dif)
  alpha <- cmod$est.a
  if (is.null(alpha)) alpha <- rep(1, J) else {
    if(length(alpha) == 1) alpha <- rep(alpha, J)
  }
  X <- mod$items
  n <- nrow(X)
  ab_hat <- rep(NA, n)
  
  zero <- function(theta, dif, alpha, x){
    sum(alpha*(x - Pj(theta, alpha, dif)))
  }
  for (i in 1:n){
    if (sum(X[i,]) %in% c(0, J)) next
    ab_hat[i] <- uniroot(zero, interval = c(-5, 5),
                         x = X[i,], alpha = alpha, dif = dif)$root
  }
  return(ab_hat)
}

# BME
bme_ab <- function(mod){
  cmod <- coef(mod)
  dif  <- cmod$est.d
  J    <- length(dif)
  alpha <- cmod$est.a

  if (is.null(alpha)) alpha <- rep(1, J) else {
    if(length(alpha) == 1) alpha <- rep(alpha, J)
  }

  X <- mod$items
  n <- nrow(X)
  
  if ('beta' %in% names(mod)){
    mu <- mod$linpred%*%mod$beta[,1]
  } else mu  <- rep(0, n)

  ab_hat <- rep(NA, n)

  zero <- function(theta, dif, alpha, x, mu_i){
    sum(alpha*(x - Pj(theta, alpha, dif))) - (theta - mu_i)
  }
  for (i in 1:n){
    ab_hat[i] <- uniroot(zero, interval = c(-5, 5),
                         x = X[i, ], alpha = alpha, dif = dif, mu_i = mu[i])$root
  }
  return(ab_hat)
}

# WLE
wle_ab <- function(mod){
  cmod <- coef(mod)
  dif  <- cmod$est.d
  J    <- length(dif)
  alpha <- cmod$est.a

  if (is.null(alpha)) alpha <- rep(1, J) else {
    if(length(alpha) == 1) alpha <- rep(alpha, J)
  }
  if('linpred' %in% names(mod)){
    reg <- TRUE
    Zb <- mod$linpred %*% cmod$est.b
  } else reg <- FALSE

  X <- mod$items
  n <- nrow(X)
  
  ab_hat <- rep(NA, n)

  zero <- function(theta, dif, alpha, x){
    pj <- Pj(theta, alpha, dif)
    I  <- sum((alpha^2)*pj*(1 - pj))
    j  <- sum((alpha^2)*pj*(1 - pj)*(1 - 2*pj))
    sum(alpha*(x - pj)) + 0.5*j/I
  }
  for (i in 1:n){
    if (reg) zbi <- Zb[i] else zbi <- 0
    ab_hat[i] <- uniroot(zero, interval = c(-5 + zbi, 5 + zbi),
                         x = X[i, ], alpha = alpha, dif = dif)$root
    if (reg) ab_hat[i] <- ab_hat[i] - zbi
  }
  return(ab_hat)
}

# ability estimation
ability <- function(mod, type = 'wle', R = 100){
  stopifnot(inherits(mod, 'rasch'))
  ab <- switch(type,
               'mle' = mle_ab(mod),
               'bme' = bme_ab(mod),
               'wle' = wle_ab(mod),
               'eap' = eap_ab(mod))
  return(ab)
}

# person fit statistic
pfs <- function(mod, level = 0.05, ab_type = 'wle'){
  stopifnot(inherits(mod, 'rasch'))
  cmod  <- coef(mod)
  delta <- cmod$est.d
  J     <- length(delta)
  alpha <- cmod$est.a
  X     <- mod$items
  n     <- nrow(X)
  
  if (is.null(alpha)) alpha <- rep(1, J) else {
    if(length(alpha) == 1) alpha <- rep(alpha, J)
  }
  
  # ability estimation
  theta_hat <- ability(mod, type = ab_type)
  
  #P(theta) calculation
  Pt <- t(sapply(theta_hat, function(x)Pj(x, alpha, delta)))
  
  # weights calculation
  wts <- log(Pt/(1-Pt))
  
  # weights correction
  rj  <- matrix(alpha, nrow = n, ncol = J, byrow = TRUE)
  pt1 <- Pt*(1-Pt)*rj
  num <- pt1*wts
  den <- pt1*rj
  cn  <- apply(num, 1, sum)/apply(den, 1, sum)
  wts <- wts - cn*rj

  # statistics
  Wn  <- apply((X - Pt)*wts, 1, sum)
  tau <- apply(Pt*(1 - Pt)*wts^2, 1, sum)
  l_z <- Wn/sqrt(tau) # falta restar la media...
  pv  <- pnorm(l_z)
  out <- data.frame(theta = theta_hat,
                    statistic = l_z,
                    p.value = pv)
  
  ggplot(out, aes(x = theta_hat, y = l_z)) +
    geom_point() +
    xlab(expression(hat(theta))) +
    ylab(expression(l[z])) +
    ggtitle('Person Fit Statistics') +
    geom_hline(yintercept = qnorm(level),
               linetype   = 'dashed',
               color      = 'tomato')
  return(out)
}

# LD
# estadistico de chan thiessen
prob_ij<-function(tita, deltas, alphas){
  eta_i <- alphas[1]*(tita - deltas[1])
  eta_j <- alphas[2]*(tita - deltas[2])
  p00 <- ( 1/(1+exp(eta_i)) )*( 1/(1+exp(eta_j)) )
  p01 <- ( 1/(1+exp(eta_i)) )*( exp(eta_j)/(1+exp(eta_j)) )
  p10 <- ( exp(eta_i)/(1+exp(eta_i)) )*( 1/(1+exp(eta_j)) )
  p11 <- ( exp(eta_i)/(1+exp(eta_i)) )*( exp(eta_j)/(1+exp(eta_j)) )
  matrix(c(p00,p01,p10,p11),2,2)
}
ch_th <- function(mod, nsim = 1e4){
  items <- mod$items
  J <- ncol(items)
  n <- nrow(items)
  cmod <- coef(mod)
  dif <- cmod$est.d
  if ('est.a' %in% names(cmod)) alphas <- cmod$est.a else alphas <- rep(1,J)
  if (length(alphas) == 1) alphas <- rep(alphas, J)
  
  X2 <- G2 <-matrix(NA, J, J)
  colnames(X2) <- rownames(X2) <- colnames(items)
  colnames(G2) <- rownames(G2) <- colnames(items)
  titas <- rnorm(nsim)
  for (i in 1:(J-1)){
    for (j in (i+1):J){
      tab_ij <- table(items[,i],items[,j])
      pij <- as.numeric(t(tab_ij/n))
      Pij <- apply(sapply(titas, prob_ij, dif[c(i,j)], alphas[c(i,j)]), 1, mean)
      X2[i,j] <-   n*sum(((pij-Pij)^2)/Pij)
      G2[i,j] <- 2*n*sum(pij*log(pij/Pij))  # se rompo cuando algun pij=0
      X2[j,i] <- 1 - pchisq(X2[i,j], 1)
      G2[j,i] <- 1 - pchisq(G2[i,j], 1)
    }
  }
  return(X2)
}


Q3 <- function(mod, ab_type = 'wle'){
  items <- mod$items
  J     <- ncol(items)
  theta <- ability(mod, type = ab_type)
  cmod  <- coef(mod)
  dif   <- cmod$est.d
  if ('est.a' %in% names(cmod)) alphas <- cmod$est.a else alphas <- rep(1,J)
  if (length(alphas) == 1) alphas <- rep(alphas, J)
  
  eta  <- outer(theta, dif, '-') %*% diag(alphas)
  res  <- items - exp(eta)/(1+ exp(eta))
  q3 <- cor(res)
  q3[lower.tri(q3, diag = TRUE)] <- NA
  z <- 0.5*(log(1 + q3) - log(1 - q3))
  z <- t(2*(1-pnorm(abs(z))))
  q3[lower.tri(q3)] <- z[lower.tri(z)]
  return(q3)
}


int_tetra <- function(w, z1, z2){
  exp(-0.5*(z1^2 + z2^2 - 2*z1*z2*cos(w))/(sin(w)^2))
}
zero_tetra <- function(r, tab){
  tab <- tab/sum(tab)
  a <- tab[1,1]
  p1 <- a + tab[1,2]
  p2 <- a + tab[2,1]
  z1 <- qnorm(p1)
  z2 <- qnorm(p2)
  integrate(int_tetra, lower = acos(r), upper = pi, z1 = z1, z2 = z2)$value/(2*pi)-a
}
tetra <- function(tab, l=-0.99, u=0.99) uniroot(zero_tetra, c(l,u), tab = tab)$root
# https://core.ac.uk/download/pdf/14378486.pdf
tetram<-function(items){
  J  <- ncol(items)
  tt <- diag(J)
  for (i in 1:(J-1)) {
    for (j in (i+1):J){
      tab_ij  <- table(items[,i], items[,j])
      if(any(tab_ij==0)) tab_ij <- tab_ij+1/2
      tl <- zero_tetra(-0.99, tab_ij)
      tu <- zero_tetra( 0.99, tab_ij)
      if(tl*tu > 0) tab_ij <- tab_ij[c(2,1),c(2,1)]
      tt[i,j] <- tetra(tab_ij)
      tt[j,i] <- tetra(tab_ij)
    }
  }
  return(tt)
}

# unidimensionality test
unidim <- function(mod, B = 99, trace = TRUE){
  extract2 <- function(mod){
    items <- mod$items
    snd <- eigen(tetram(items))$values[2]
  }
  stat <- extract2(mod)
  mods <- pbootr(mod, B = B, trace = trace)
  statb <- unlist(lapply(mods, extract2))
  pval  <- (1 + sum(statb > stat))/(B + 1)
  cat('Bootstrap based unidimensionality test','\n')
  print(data.frame(statistic = stat,
                   p.value = pval))
  invisible(list(statistic = stat, p.value = pval))
}
# Drasgow, F. and Lissak, R. (1983) Modified parallel analysis: a procedure for examining the latent dimensionality of dichotomously scored item responses. Journal of Applied Psychology, 68, 363-373.

# Likelihoood Ratio based Confidenc Interval for proportions
lr_ci_p <- function(x, n, level = 0.95){
  # Exact likelihood ratio and score confidence intervals for the binomial proportion
  z    <- qchisq(level, 1)
  den  <- 2*(n + z^2)
  A <- (2*x + z^2)/den
  B <- z*sqrt(z^2 + 4*x*(1 - x/n))/den
  linf <- A - B
  lsup <- A + B
  return(c(linf, lsup))
}
