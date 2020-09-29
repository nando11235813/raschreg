# plot rasch object (Item Characteristic Curves)
plot.rasch <- function(x, ..., xlim = c(-4, 4), item = NULL, main = NULL){
  stopifnot(inherits(x, 'rasch'))
  theta   <- matrix(seq(xlim[1], xlim[2], 0.01), ncol = 1)
  pnames  <- unlist(lapply(strsplit(rownames(x$coef), '_'), function(x)x[1]))
  pvnames <- rownames(x$coef)
  J <- ncol(x$items)
  if('alpha' %in% pnames) {
    a <- x$coef[which(pnames=='alpha'), 1]
    if(length(a)==1) a <- rep(a, J)
  }  else a <- rep(1,J)
  if('gamma' %in% pnames) pg <- x$coef[which(pnames=='gamma'), 1] else pg<-rep(0, J)
  d <- x$coef[which(pnames=='delta'), 1]
  for (j in 1:J){
    prob  <- pg[j] + (1 - pg[j])/(1 + exp(-a[j]*(theta[, 1] - d[j])))
    theta <- cbind(theta, prob)
  }
  colnames(theta) <- c('theta', colnames(x$items))
  its   <- colnames(x$items)
  theta <- reshape(as.data.frame(theta),
                   varying   = its,
                   direction = 'long',
                   v.names   = 'valor',
                   timevar   = 'variable',
                   times     = its)

  # color ordering by item difficulty
  theta$group <- factor(theta$variable,
                        levels = unique(theta$variable)[order(x$coef[,1])])
  
  # color  just identified item
  if (!is.null(item)){
    if (!item %in% its) stop('Wrong item name')
    theta$color <- ifelse(theta$group == item, item, 'other items')
  } else theta$color <- theta$group
  
  if (is.null(main)) main <- 'Item Characteristic Curves'
  
  p1 <- ggplot(data = theta,
               aes(x     = .data$theta,
                   y     = .data$valor,
                   group = .data$group,
                   col   = .data$color)) + 
          geom_line(size = 1.3) + 
          ylab(expression(P(Y[j]*" | "*theta))) + 
          xlab(expression(theta)) + 
          ggtitle(main) + 
          geom_hline(yintercept = c(0,1)) + 
          geom_vline(xintercept = 0)
  if (!is.null(item)){
    p1 <- p1 + 
      scale_color_manual(values=c('navy','grey80')) +
      theme_bw()
  }
  p1 <- p1 + theme(legend.title = element_blank())
  print(p1)
}

# plot Information Curves
info <- function(mod, theta = NULL, item = NULL, main_item = NULL, main_total = NULL, which = 'both', plot = TRUE){
  if (is.null(theta)) {
    theta <- matrix(seq(-4, 4, 0.01), ncol=1) 
  } else {
    theta <- matrix(theta, ncol = 1)
  }
  d       <- mod$coef[, 1]
  pnames  <- unlist(lapply(strsplit(rownames(mod$coef), '_'), function(x)x[1]))
  pvnames <- rownames(mod$coef)
  J       <- ncol(mod$items)
  if ('alpha' %in% pnames){
    a <- mod$coef[which(pnames == 'alpha'), 1]
    if (length(a) == 1) a <- rep(a, J)
    those <- which(pnames == 'alpha')
    mod$coef <- mod$coef[-those, ]
  } else a   <- rep(1, J)
  if ('gamma' %in% pnames){
    those    <- which(pnames == 'gamma')
    pg       <- mod$coef[those, 1]
    mod$coef <- mod$coef[-those, ]
  } else pg  <- rep(0, J)
  for (i in 1:J){
    p1p   <- (1 - pg[i])*(a[i]^2)*(1/(1 + exp( -(theta[, 1] - d[i]))))*(1/(1 + exp(theta[, 1] - d[i])))*(exp(theta[, 1] - d[i]))/(pg[i] + exp(theta[, 1] - d[i]))
    theta <- cbind(theta, p1p)
  }
  colnames(theta) <- c('theta', colnames(mod$items))
  theta0          <- cbind(theta, Total = apply(theta[,-1], 1, sum))

  its   <- c(colnames(mod$items), 'Total')
  theta <- reshape(as.data.frame(theta0),
                   varying   = its,
                   direction = 'long',
                   v.names   = 'valor',
                   timevar   = 'variable',
                   times     = its)
  
  # color order by item difficulty
  theta$group <- factor(theta$variable, levels = unique(theta$variable)[c(order(d), length(d) + 1)])

  # color  just identified item
  if (!is.null(item)){
    if (!item %in% its) stop('Wrong item name')
    theta$color <- ifelse(theta$group==item,item,'other items')
  } else theta$color <- theta$variable
  
  if (is.null(main_item))  main_item <- 'Item Information Curves'
  if (is.null(main_total)) main_total <- 'Total Information Curve'
  
  itemI <- ggplot(theta[theta$group != 'Total', ],
                  aes(x     = .data$theta,
                      y     = .data$valor,
                      group = .data$group,
                      col   = .data$color)) +
       geom_line(size = 1.3) + 
       ylab(expression(I[j](theta))) +
       xlab(expression(theta)) +
       ggtitle(main_item) +
       geom_vline(xintercept = 0) + 
       geom_hline(yintercept = 0)
  if(!is.null(item)){
    itemI <- itemI + 
      scale_color_manual(values=c('navy','grey80')) +
      theme_bw()
  }
  itemI <- itemI + theme(legend.title = element_blank())
  
  totalI <- ggplot(theta[theta$variable=='Total',],
                   aes(x = .data$theta,
                       y = .data$valor)) +
       geom_line(size = 1.3,
                 col  = 'black') + 
       ylab(expression(I[Tot](theta))) +
       xlab(expression(theta)) +
       ggtitle(main_total) +
       geom_vline(xintercept = 0) + 
       geom_hline(yintercept = 0)
     p <- plot_grid(itemI, totalI, nrow = 1)
     if (plot){
       if (which == 'both')  print(p)
       if (which == 'item')  print(itemI)
       if (which == 'total') print(totalI)
     } 
  invisible(theta0)
}

# Person Item Map
pim <- function(mod, main = NULL){
  stopifnot(inherits(mod, 'rasch'))
  cd <- coef(mod)
  d  <- cd$est.d
  J  <- ncol(mod$items)
  
  # ability precition
  theta <- ability(mod)
  theta <- theta[, ncol(theta)]
  
  items <- colnames(mod$items)
  tab_theta <- table(round(theta,3))
  tab_theta <- data.frame(estimate = sort(unique(round(theta, 3))),
                          freq     = as.numeric(tab_theta/sum(tab_theta)))
  dif       <- data.frame(item      = items,
                          estimate  = d,
                          freq      = 0,
                          disc      = 1,
                          row.names = NULL)
  if('est.a' %in% names(cd)) dif$disc <- cd$est.a
  M <- max(tab_theta$freq)
  
  if (is.null(main)) main <- 'Person-item map'
  
  g<-ggplot(dif, 
            aes(x = .data$freq,
                y = .data$estimate)) +
    geom_point(aes(col = 'red', size = .data$disc)) +
    geom_vline(xintercept = 0) +
    geom_text(aes(label = .data$item),
              hjust     = "right",
              nudge_x   = -M/50) +
    geom_rect(data = tab_theta,
              mapping = aes(xmin = 0,
                            xmax = .data$freq,
                            ymin = .data$estimate-0.05,
                            ymax = .data$estimate+0.05)) +
    theme(axis.title.x = element_blank(),
          axis.text.x  = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "none") +
    xlim(-M/4,M) +
    ylab('Ability scale') +
    ggtitle(main)
  print(g)
}

# Forest plot
forest <- function(mod, level = 0.05, main_dif = NULL, main_disc = NULL, main_reg = NULL){
  stopifnot(inherits(mod, 'rasch'))
  s      <- summary(mod)
  graphs <- vector('list', 0)
  
  # difficulty  forest
  dof      <- nrow(mod$items) - length(unlist(coef(mod)))
  tval     <- qt(1 - level/2, dof)
  sdi      <- s$difficulty[,1:2]
  sdi$linf <- sdi[, 1] - tval*sdi[, 2]
  sdi$lsup <- sdi[, 1] + tval*sdi[, 2]
  # item names
  sdi$items <- unlist(lapply(strsplit(rownames(sdi), '_'), function(x)x[2]))
  
  if (is.null(main_dif)) main_dif <- 'Forest plot of difficulty parameters'
  
  graphs$g1 <- ggplot(data = sdi,
                      aes(x   = .data$Estimate,
                          y   = .data$items,
                          col = .data$items)) +
      geom_errorbarh(aes(xmin = .data$linf,
                         xmax = .data$lsup),
                     height = 0.1,
                     size   = 1) +
      geom_point(size = 2) +
      ylab('') + 
      xlab('') + 
      ggtitle(main_dif) + 
      geom_vline(xintercept = 0) +
      theme_classic() +
      theme(legend.position = 'none')

  # discrimination forest
  if ('alpha' %in% names(s)) {
    sda      <- s$alpha[,1:2]
    if (nrow(sda) == 1){
      sda[1:ncol(mod$items), ] <- sda
      rownames(sda) <- rownames(sdi)
    }
    sda$linf <- sda[, 1] - tval*sda[, 2]
    sda$lsup <- sda[, 1] + tval*sda[, 2]
    # item names
    sda$items <- unlist(lapply(strsplit(rownames(sda), '_'), function(x)x[2]))
    
    if (is.null(main_disc)) main_disc <- 'Forest plot of discrimination parameters'
    
    graphs$g2 <- ggplot(data    = sda,
                        aes(x   = .data$Estimate,
                            y   = .data$items,
                            col = .data$items)) +
      geom_errorbarh(aes(xmin = .data$linf,
                         xmax = .data$lsup),
                     height = 0.1,
                     size   = 1) +
      geom_point(size = 2) +
      ylab('') + 
      xlab('') + 
      ggtitle(main_disc) + 
      geom_vline(xintercept = 1) +
      theme_classic() +
        theme(legend.position = 'none')
  }

  # regression forest
  if ('beta' %in% names(s)) {
    sdb      <- s$beta[,1:2]
    sdb$linf <- sdb[, 1] - tval*sdb[, 2]
    sdb$lsup <- sdb[, 1] + tval*sdb[, 2]
    # item names
    sdb$variables <- rownames(sdb)
    
    if (is.null(main_reg)) main_reg <- 'Forest plot of regression parameters'
  
      graphs$g3 <- ggplot(data=sdb,
                        aes(x   = .data$Estimate,
                            y   = .data$variables,
                            col = .data$variables)) +
      geom_errorbarh(aes(xmin = .data$linf,
                         xmax = .data$lsup),
                     height = 0.1,
                     size   = 1) +
      geom_point(size = 2) +
      ylab('') + 
      xlab('') + 
      ggtitle(main_reg) + 
      geom_vline(xintercept = 0) +
      theme_classic() +
      theme(legend.position = 'none')
  }
  plot_grid(plotlist = graphs, nrow = 1)
}

# plotfit
itemfit <- function(mod, item, xlim = c(-3, 3), col = 'tomato', main = NULL){
  est  <- coef(mod)
  delta <- unlist(est$est.d)
  J <- length(delta)
  if('est.a' %in% names(est)) alpha <- est$est.a else alpha <-rep(1, J)
  
  # ability
  lv <- ability(mod)
  lv <- round(lv[,'thetac'],3)
  
  its    <- as.data.frame(mod$items)
  its$lv <- lv
  phat   <- aggregate(its[, 1:J],data.frame(lv=its$lv), mean, na.rm = TRUE)
  
  # item selection
  ncol <- which(colnames(its) == item)
  dat <- data.frame(lv =phat$lv, p_theta = phat[,ncol])

  
  # ICC
  theta   <- seq(xlim[1], xlim[2], 0.01)
  p_theta <- (1/(1 + exp(-alpha[ncol - 1]*(theta - delta[ncol - 1]))))
  dat0    <- data.frame(lv = theta, p_theta)
  
  ggplot(dat0, aes(x = lv, y = p_theta)) +
    geom_line(col = col, size = 1) +
    geom_point(data = dat) +
    ylim(0, 1) + 
    xlab(expression(theta)) + ylab(expression(P(theta))) +
    geom_hline(yintercept=c(0, 1)) +
    geom_vline(xintercept=c(0)) +
    ggtitle(main)
}
