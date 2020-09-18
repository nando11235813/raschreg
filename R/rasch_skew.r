raschsk<-function(items, init = NULL){

  if (class(items) == 'data.frame') items <- as.matrix(items)
  # initial values
	J <- ncol(items)
	if (is.null(init)) init<-rep(0,J+1)

	# NA checking
	if(any(is.na(items))) {
		na    <- apply(is.na(items), 1, sum)
		items <- items[ -which(na != 0), ]
		print(paste(sum(na != 0), ' rows were removed', sep = ''))
	}
  par <- nlminb(start     = init,
                objective = raschlikLA_skew,
                X         = items,
                control   = list(rel.tol = 1e-5,
                                 x.tol   = 1e-5))
	
 	# approximate observed information matrix
	H<-hessian(fun   = raschlikLA_skew,
	           param = par$par,
	           X     = items,
	           fun0  = par$objective)
	V  <- solve(H)
	rownames(V) <- colnames(V) <- c(paste('delta', colnames(items), sep = '_'),'asymm')
	se      <- sqrt(diag(V))
	nu      <- par$par[J + 1]
	# standard error of asymmetry parameter (delta method)
	se[J+1] <- se[J + 1]*2*exp( -nu)/((1 + exp( -nu))^2)
	nu      <- (1 - exp( -nu ))/(1 + exp( -nu ))
	
	loglik<- par$objective
	iter  <- par$iterations
	dof   <- nrow(items) - length(par$par)
	parms <- data.frame(c(par$par[1:J],nu),
	                    se,
	                    par$par/se,
	                    2*(1 - pt(abs(par$par/se), df = dof)))
	colnames(parms) <- c('Estimate',
	                     'Std. Error',
	                     't value',
	                     'Pr(|>t|)')
	rownames(parms) <- rownames(V)

	call <- match.call()
	mod  <- list(call   = call,
	             coef   = parms,
	             iter   = iter,
	             loglik = -loglik,
	             vcov   = V,
	             items  = items)
	class(mod) <- append(class(mod), 'rasch')
	return(mod)
}