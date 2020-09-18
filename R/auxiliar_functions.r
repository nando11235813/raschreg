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
summary.rasch <- function(object, correlation = FALSE, signif.stars = getOption("show.signif.stars"), ...){
  stopifnot(inherits(object, 'rasch'))
  out <- list()
  
  cat(paste('n',      nrow(object$items),     sep='      '), '\n')
  cat(paste('logLik', round(object$loglik,3), sep=' ')     , '\n') 
  cat(paste('AIC',    round(AIC(object), 3), sep='    ')   , '\n')
  cat(paste('BIC',    round(BIC(object), 3), sep='    ')   , '\n')
  out$AIC    <- AIC(object)
  out$BIC    <- BIC(object)
  out$logLik <- object$loglik
  
  pnames  <- unlist(lapply(strsplit(rownames(object$coef), '_'),function(x) x[1]))
  pvnames <- rownames(object$coef)
  
  if ('alpha' %in% pnames){
    cat('\n')
    cat('Discrimination parameters','\n')
    a  <- round(object$coef[which(pnames == 'alpha'),],3)
    if(any(a[, 4] <= 0.001)) a[a[, 4] == 0,4]<-'<0.001'
    ss <- ifelse(a[,4]>0.1, '', 
                 ifelse(a[, 4] > 0.05, '.', 
                        ifelse(a[, 4] > 0.01, '*', 
                               ifelse(a[, 4] > 0.001, '**', 
                                      '***'))))
    names(a) <- c('Estimate',
                  'Std. Error',
                  't value',
                  'Pr(>|t|)')
    if (signif.stars) {
      a <- data.frame(a, ss, check.names = FALSE)
      names(a)[ncol(a)] <- ''
    }
    print(a)
    object$coef <- object$coef[-which(pnames == 'alpha'), ]
    out$alpha   <- a
  }
  cat('\n')
  cat('Difficulty parameters','\n')
  s  <- round(object$coef, 3)
  if(any(s[,4] <= 0.001)) s[which(s[,4] <= 0.001), 4] <- '<0.001'
  ss <- ifelse(s[, 4] > 0.1, '',
               ifelse(s[, 4] > 0.05, '.',
                      ifelse(s[, 4] > 0.01, '*',
                             ifelse(s[, 4] > 0.001, '**',
                                    '***'))))
  names(s) <- c('Estimate',
                'Std. Error',
                't value',
                'Pr(>|t|)')
  if (signif.stars) {
    s <- data.frame(s, ss, check.names=FALSE)
    names(s)[ncol(s)] <- ''
  }
  print(s)
  cat('___', '\n')
  print("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1")
  out$difficulty <- s
  
  if ('beta'%in%names(object)){
    cat('___', '\n')
    cat('Regression parameters', '\n')
    s2 <- round(object$beta, 3)
    if(any(s2[, 4] <= 0.001)) s2[s2[, 4] == 0,4] <- '<0.001'
    ss <- ifelse(s2[, 4] > 0.1,'',
                 ifelse(s2[, 4] > 0.05, '.',
                        ifelse(s2[, 4] > 0.01, '*',
                               ifelse(s2[, 4] > 0.001, '**',
                                      '***'))))
    names(s2)<-c('Estimate',
                 'Std. Error',
                 't value',
                 'Pr(>|t|)')
    if (signif.stars) {
      s2 <- data.frame(s2, ss, check.names = FALSE)
      names(s2)[ncol(s2)] <- ''
    }
    print(s2)
    cat('___','\n')
    print("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1")
    out$beta <- s2
  }
  if (correlation == TRUE){
    corr<-cov2cor(vcov(object))
    cat('___', '\n')
    print(corr, 2)
    out$correlation <- corr
  }
  invisible(out)
}

# vcov
vcov.rasch <- function(object, ...){
  stopifnot(inherits(object, 'rasch'))
  return(object$vcov)
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
    ci <- profile(object, alpha = 1 - level)
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
  f_reg  <- formula(paste('~', paste(rownames(object$beta), collapse = '+'), '-1',sep = ''))
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
	  a        <- mod$coef[which(pnames == 'alpha'), 1]
	  mod$coef <- mod$coef[-which(pnames == 'alpha'),]
	  if(length(a )== 1) a <- rep(a, J)
	} else a <- rep(1, J)
	if ('gamma' %in% pnames){
	  pg       <- mod$coef[which(pnames == 'gamma'), 1]
	  mod$coef <- mod$coef[-which(pnames == 'gamma'), ]
	} else pg  <-rep(0, J)
  delta <- mod$coef[, 1]
	X     <- mod$items
	Y     <- mod$linpred
	# b max
	if ('beta' %in% names(mod)){
	  J    <- length(delta)
	  beta <- mod$beta[, 1]
	  bmax <- apply(data.frame(X, Y),
	                1,
	                function(rowi)optimize(huc,
	                                       interval = c(-4, 4),
	                                       x = rowi[seq(J)],
	                                       d = delta,
	                                       a = a,
	                                       beta = beta,
	                                       z = rowi[-seq(J)],
	                                       tol = 1e-4 ,
	                                       maximum=TRUE)$maximum)
	  # aca podria ir otro if si llega a marchar "raschguessreg"
	} else {
	  if ('gamma' %in% pnames){
	    bmax <- apply(X, 1,function(rowi)optimize(h1plgc,
	                                         interval = c(-4,4),
	                                         x = rowi,
	                                         d = delta,
	                                         g = pg,
	                                         tol = 1e-4,
	                                         maximum = TRUE)$maximum)
	  }
	  else {
	    bmax <- apply(X, 1, function(rowi)optimize(hbc,
	                                         interval = c(-4,4),
	                                         x = rowi,
	                                         a = a,
	                                         d = delta,
	                                         tol = 1e-4,
	                                         maximum = TRUE)$maximum)
	  }
	}
	re <- data.frame(theta = bmax)
	if (type=='lord'){
	  re$bias   <- bias(mod, theta = re$theta)
	  re$thetac <- re$theta + re$bias
	}
	return(re)
}

# bias
bias <- function(mod, theta){
  abs  <- info(mod,
               plot  = FALSE,
               theta = theta)
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
	m       <- length(knots) + 2

	pCs <- matrix(NA,
	              nrow = length(new_x),
	              ncol = m - 2)
	for (i in 1:(m - 2)) pCs[, i] <- apply(cbind(0, (new_x-  knots[i])^3), 1, max) - apply(cbind(0, (new_x - knots[m - 1])^3), 1, max)*(knots[m] - knots[i])/(knots[m] - knots[m - 1]) + apply(cbind(0, (new_x - knots[m])^3), 1, max)*(knots[m-1] - knots[i])/(knots[m] - knots[m - 1])
	pCs <- cbind(new_x,pCs)
	colnames(pCs) <- paste('C', 0:(m - 2), sep = '')
	if(scale) {
		sds <- apply(object ,2, sd)
		pCs <- scale(pCs, center = FALSE, scale = sds)
	}
	attr(Cs,'class')          <- c('rcs', 'object', 'matrix')
	attr(Cs,'intercept')      <- FALSE
	attr(Cs,'Boundary.knots') <-range(new_x)
	attr(Cs,'knots')          <-knots

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
  npars <- unlist(lapply(coefs,
                         function(x) nrow(x)))
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
profile <- function(mod, alpha = 0.05){
  est  <- coef(mod)
  init <- unlist(est$est.d)
  if('est.a' %in% names(est)) init <- c(init, log(est$est.a))
  L0   <- mod$loglik - qchisq(1 - alpha/2, 1)/2
  
  items <- mod$items
  stz   <- abs(sum(est$est.d)) < 1e-7
  npar  <- length(init)
  CI    <- matrix(0, ncol = 2, nrow = npar)
  
  #log-likelihood function
  flik <- as.character(mod$call[[1]])
  flik <- paste(flik, 'likLA', sep = '')

  fzero <- function(h){
    initL   <- init
    fixL    <- rep(NA, npar)
    fixL[i] <- initL[i] <- initL[i] + h
    par <- nlminb(start     = initL,
                  objective = get(flik),
                  X         = items,
                  control   = list(rel.tol = 1e-5,
                                   x.tol   = 1e-5),
                  stz       = stz,
                  fixed     = fixL)
    -par$objective - L0
  }
  
  # free parameters
  free <- 1:npar
  if (any(is.na(mod$coef[,2]))){
    free <- free[-which(is.na(mod$coef[,2]))]
  }
  
  cat('Profiling the likelihood ...','\n')
  for (i in free){
    # left side
    CI[i,1] <- init[i] + uniroot(fzero, interval = c(-5, 0))$root
    # right side
    CI[i,2] <- init[i] + uniroot(fzero, interval = c(0, 5))$root
    print(names(init)[i])
  }
  
  rownames(CI) <- rownames(mod$coef)
  colnames(CI) <- paste(round(c(alpha/2, 1 - alpha/2)*100, 1), '%')
  J <- ncol(mod$items)
  if('est.a' %in% names(est)){
    p <- length(est$est.a)
    CI[(J + 1):(J + p), ] <- exp(CI[(J + 1):(J + p), ])
  }
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
  modr   <- update(mod, fixed = restr)
  paramr <- modr$coef[, 1]
  if ('beta' %in% names(modr)) paramr <- c(paramr, mod$beta[, 1])

  # log-likelihood function
  flik <- as.character(mod$call[[1]])
  flik <- paste(flik, 'likLA', sep = '')
  
  # score vector
  score <- gradient(fun   = get(flik),
                    param = paramr,
	                  X     = mod$items,
	                  stz   = FALSE,
	                  fixed = restr*NA)
  hess  <-  hessian(fun   = get(flik),
                    param = paramr,
	                  X     = mod$items,
	                  stz   = FALSE,
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