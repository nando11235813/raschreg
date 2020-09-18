residuals.rasch <- function(object, ..., type = 'raw'){
  type  <- match.arg(type, c('raw','std'))
  theta <- ability(object)
  theta <- theta[, ncol(theta)]
  
  if('beta' %in% names(object)){
    xb    <- object$linpred %*% object$beta[, 1]
    theta <- theta + xb[, 1]
  }
  
  items <- object$items
  theta_delta <- outer(theta, object$coef[, 1], '-')
  probs <- 1/(1 + exp( -theta_delta))
  
  res   <- items - probs
  if (type=='std'){
    W <- probs * (1 - probs)
    res <- res / sqrt(W)
  }
  
  return(res)
}

itemfit <- function(mod, plot = TRUE){
  r      <- residuals(mod, type = 'raw')
  P      <- mod$items - r
  W      <- P * (1 - P)
  rstd   <- r/sqrt(W)
  # standarized squared residuals
  sq_r   <- rstd^2
  
  # raw statistics
  outfit   <- apply(sq_r, 2, mean, na.rm = TRUE)
  infit    <- apply(W*sq_r , 2, sum,  na.rm = TRUE)/apply(W, 2, sum, na.rm = TRUE)
  
  # standarized statistics
  K<- W*(1-3*W)
  n  <- nrow(r)
  qout2 <- apply(K/(W^2), 2, sum)/(n^2) - 1/n 
  qin2  <- apply(K - W^2, 2, sum)/(apply(W, 2, sum)^2)
  qin   <- sqrt(qin2)
  qout  <- sqrt(qout2)
  # los amigos del mirt truncan estos cosos en sqrt(2)
  qin[qin>sqrt(2)]   <- sqrt(2)
  qout[qout>sqrt(2)] <- sqrt(2)
  outfit_t <- (outfit^(1/3) - 1)*(3/qout) +  (qout/3)
  infit_t  <- ( infit^(1/3) - 1)*(3/qin) + (qin/3)
  
  fits   <- data.frame(item     = colnames(r),
                       infit    = infit,
                       infit_t  = infit_t,
                       outfit   = outfit,
                       outfit_t = outfit_t)
  if (plot){
    J     <- ncol(r)
    delta <- mod$coef[1:J,1]
    fits$delta <- delta
    I <- ggplot(data = fits,
                aes(x     = .data$infit_t,
                    y     = .data$delta,
                    label = .data$item)) +
          geom_point() +
          geom_text(check_overlap = TRUE) +
          xlab('Standarized Infit') + ylab('Item difficulty') +
          geom_vline(xintercept = c(-2, 2),
                     linetype   = 'dashed',
                     color      = 'tomato')
    O <- ggplot(data = fits,
                aes(x     = .data$outfit_t,
                    y     = .data$delta,
                    label = .data$item)) +
          geom_point() +
          geom_text(check_overlap = TRUE) +
          xlab('Standarized Outfit') + ylab('Item difficulty') +
          geom_vline(xintercept = c(-2, 2),
                     linetype   = 'dashed',
                     color      = 'tomato')
    p <- plot_grid(I, O, nrow = 1)
    print(p)
  }
  
  return(list(stats=fits,kurt=K,var=W,mean=P))
}
