### SAR model with SSVS prior and beta prior for rho ###
# rm(list=ls())
# install MASS package for multivariate normal random number
if (!require("MASS")) install.packages("MASS")
require(psych)
library(FNN)
library(spam)
library(Matrix)
library(pracma)
# Fast logdet approximation function

### first let's construct our SAR DGP
# contains function to contruct n-nearest neighbour spatial weight matrix

# n=1155
# smallk = 3
# # 7 nearest neighbour W construction from random pattern
# xy <- cbind(runif(n),runif(n))
# W<-getWknn(xy,7)
# #X <- cbind( 1, rnorm(n), rnorm(n) )
# X <- as.data.frame(matrix(rnorm(n*smallk),ncol=smallk))
# X = scale(X,scale = FALSE,center = TRUE)
# X.full = cbind(1, X,W %*% X)
# RHO = .35
# BETA <- round(rnorm(1+smallk*2),2)
# SIGMA = 1
# AI_ = solve(diag(n) - RHO * W)
# Y = AI_ %*% (X.full %*%  BETA + rnorm(n, mean = 0,sd = SIGMA) )
#
# # DIRECT = sum(diag(AI_))/n * BETA[1:4] + c(0,sum(diag(AI_ %*% W))/n * BETA[5:7])
# # TOTAL = sum(AI_)/n * BETA[1:4] + c(0,sum(AI_ %*% W)/n * BETA[5:7])
# # INDIRECT =TOTAL - DIRECT
#
# k = 7
#
# test <- SDM_SSVS(Y,X,W)
#
# tt = 1
# niter = 200
# nretain = 100
# ssvs.w = rep(0.5, ncol(X) * 2 )
# rho_a = 1.1
# A0 = 10^8
# sigma_a = .001
# sigma_b = .001
# Z = NULL

SDM_SSVS = function(Y,X,W,Z = NULL,tt = 1,
               niter = 2000,nretain = 1000,
               A0 = 10^8,sigma_a = .001,sigma_b = .001,
               ssvs.w = c(1, rep(0.5, ncol(X) * 2 + ifelse(is.null(Z),0,ncol(Z)))),
               rho_a = 1.1) {
   WW = kronecker(.sparseDiagonal(tt),W)
   if (is.null(X)) {
     n = nrow(Z) / tt
     smallk = ncol(Z)
     smallk2 = smallk + 1
     origX = X
     xnames = c("(Intercept)",colnames(Z))
     X = cbind(1,Z)
  } else {
    n = nrow(X) / tt
    smallk = ncol(X)
    origX = X
    xnames = c("(Intercept)",colnames(origX))
    X = cbind(1,X, as.matrix(WW %*% X))
    if (!is.null(Z)) {
      X = cbind(X,Z)
    }
    smallk2 = smallk * 2 + 1
  }
  k = ncol(X)

  ### let us assign prior values
  # beta mean and variance are proper, but with high variance (= low prior information)
  beta_prior_mean = matrix(0,k,1)
  beta_prior_var = diag(k) * A0
  # rho prior is beta, with
  beta_prob = function(rho,a) 1/beta(a,a) * ((1+rho)^(a-1) *(1-rho)^(a-1))/(2^(2*a - 1))


  ### calibration parameters for rho sampling
  cc = 1 #scaling of rho proposals
  c_adjust = 1.1 #proposal distribution adjustment
  rho_accept = 0 #counter for rho acceptance rates

  ### set-up the gibbs sampler
  # total number of draws
  ndiscard = niter - nretain
  # save the posterior draws here
  postb = matrix(0,k,nretain)
  postg = matrix(0,k,nretain)
  posts = matrix(0,1,nretain)
  postr = matrix(0,1,nretain)
  rtmp = matrix(0,1,niter) # rho acceptance rates
  post.direct = matrix(0,smallk + 1,nretain)
  post.indirect = matrix(0,smallk + 1,nretain)
  post.total = matrix(0,smallk + 1,nretain)
  dimnames(post.direct)[[1]] =
    dimnames(post.indirect)[[1]] =
    dimnames(post.total)[[1]] = xnames

  tY = matrix(Y,n,tt)
  WY = matrix((W %*% tY),n*tt,1)

  # Make the semi-automatic choices of "small" and "large" prior variances
  # as recommended by George, Sun and Ni (2008, JOE)
  # This uses OLS so will not work if bigk>nobs
  OLSX = cbind(WY,X)
  xtxinv = solve(crossprod(OLSX));
  b_ols = xtxinv %*% crossprod(OLSX,Y)
  s2_ols = as.double(crossprod(Y- OLSX %*% b_ols)/(n - k-1))
  b_cov = s2_ols*xtxinv;
  b_sd = sqrt(diag(b_cov));

  #prior hyperparameters
  ssvs_constant = 5
  c0 = 1/ssvs_constant
  c1 = ssvs_constant
  tau0 = c0*b_sd[-1]
  tau1 = c1*b_sd[-1]

  #ssvs.w = rep(0.5,k) #prior inclusion probability
  #ssvs.w[1] = 1 # always include the intercept

  # set-up for griddy gibbs
  griddy_n = 100
  logdets = lndetPaceBarry(as.matrix(W),length.out = griddy_n+2)
  logdets = logdets[-c(1,griddy_n + 2),]
  rrhos = logdets[,2]
  ## storage for efficient partial derivatives
  ai_diags = rep(0,griddy_n)
  ai_tots = rep(0,griddy_n)
  aiW_diags = rep(0,griddy_n)
  aiW_tots = rep(0,griddy_n)
  cat("Pre-calculate griddy GIBBS...")
  for (ii in 1:griddy_n) {
    A = (.sparseDiagonal(n) - rrhos[ii] * W)
    AI = solve(A)
    ai_diags[ii] = sum(diag(AI))
    ai_tots[ii] = sum(AI)
    aiW_diags[ii] = sum(diag(AI %*% W))
    aiW_tots[ii] = sum(AI %*% W)
  }
  cat("Done!\n")

  # starting values (won't matter after sufficient draws)
  curr.beta = b_ols[-1]
  curr.sigma = s2_ols
  curr.rho = 0
  curr.gamma = rep(1,k)

  # pre-calculate some terms for faster draws
  #beta_prior_var_inv = solve(beta_prior_var)
  XpX = t(X) %*% X
  curr.Ay = Y - curr.rho*WY
  V.prior <- diag(curr.gamma)

  ### Gibbs sampling
  pb <- txtProgressBar(min = 0, max = niter, style = 3)
  iter <- 1
  for (iter in 1:niter) {
    #cat("iter:",iter,"curr.rho:",curr.rho,"\n")

    # # sampling mixture component curr.gamma
    j <- 3
    for (j in 1:k){
      a0 <- dnorm(curr.beta[j],0,tau0[j],log=TRUE)
      a1 <- dnorm(curr.beta[j],0,tau1[j],log=TRUE)
      probs <- (exp(a0) * (1-ssvs.w[j])  )   /
        sum(  exp(a0) * (1-ssvs.w[j])     +  exp(a1) * ssvs.w[j])
      if (runif(1)>probs){
        V.prior[j,j] <- (tau1[j])^2
        curr.gamma[j] <- 1
      }else{
        V.prior[j,j] <- (tau0[j])^2
        curr.gamma[j] <- 0
      }
    }


    # draw beta
    #V = solve(beta_prior_var_inv + 1/curr.sigma * XpX )
    #b = V %*% (beta_prior_var_inv%*%beta_prior_mean + 1/curr.sigma * t(X) %*% curr.Ay )
    V = solve(diag(1/diag(V.prior)) + 1/curr.sigma * XpX )
    b = V %*% (1/curr.sigma * t(X) %*% curr.Ay )
    curr.beta = mvrnorm(1,b,V)

    # draw sigma
    curr.xb = X %*% curr.beta
    curr.ESS = crossprod(curr.Ay - curr.xb)
    curr.sigma = 1/rgamma(1, sigma_a + (n*tt)/2, sigma_b + as.double(curr.ESS) / 2)

    ## Griddy-Gibbs step for rho
    V = solve(diag(1/diag(V.prior)) + 1/curr.sigma * XpX )
    b0 = V %*% (1/curr.sigma * t(X) %*% Y )
    bd = V %*% (1/curr.sigma * t(X) %*% WY)
    e0 = Y - X %*% b0
    ed = WY - X %*% bd
    epe0 = as.double(t(e0) %*% e0)
    eped = as.double(t(ed) %*% ed)
    epe0d = as.double(t(ed) %*% e0)
    z = epe0  - 2 * rrhos * epe0d + rrhos^2 * eped
    z = -(n*tt-k)/2 * log(z)
    den = logdets[,1] + z + log(beta_prob(rrhos,rho_a))
    y = rrhos
    adj = max(den)
    den = den - adj
    x = exp(den)
    isum = sum((y[-1] + y[-length(y)])*(x[-1]  - x[-length(x)])/2)
    z = abs(x/isum)
    den = cumsum(z)
    rnd = runif(1) * sum(z)
    ind = max(which(den <= rnd))
    if (is.integer(ind) && ind <= length(rrhos)) {
      curr.rho = rrhos[ind]
      curr.Ay = Y - curr.rho*WY
      curr.ai_diag = ai_diags[ind]
      curr.ai_tot = ai_tots[ind]
      curr.aiW_diag = aiW_diags[ind]
      curr.aiW_tot = aiW_tots[ind]
    }

    # ## Metropolis-Hastings step for rho
    # # logdet calculation costly, so do it as few times as possible
    # if (iter == 1) {curr.logdet = log(det(diag(n) - curr.rho*W))}
    # curr.llh = curr.logdet - as.double(curr.ESS)/ (2*curr.sigma) + beta_prob(curr.rho,rho_a)
    # accept = 0;
    # while (accept!=1) {
    #   prop.rho = curr.rho + cc*rnorm(1,0,1)
    #   if (prop.rho<1 && prop.rho>-1) {
    #     accept = 1
    #   }
    # }
    # prop.Ay = Y - prop.rho*WY
    # prop.ESS = t(prop.Ay - curr.xb) %*% (prop.Ay - curr.xb)
    # prop.logdet = log(det(diag(n) - prop.rho*W))
    # prop.llh =  prop.logdet - as.double(prop.ESS)/ (2*curr.sigma) + beta_prob(prop.rho,rho_a)
    # acc_prob = min(1,exp(prop.llh - curr.llh))
    # if (rbinom(1,1,acc_prob) == 1) {
    #   curr.rho = prop.rho
    #   rho_accept = rho_accept + 1
    #   curr.logdet = prop.logdet
    #   curr.Ay = prop.Ay
    # }
    # # adjust candidate distribution based on acceptance probability
    # rtmp[iter] = rho_accept/iter
    # if (iter < ndiscard/2) {
    #   if (rtmp[iter]<.4) {
    #     cc <- cc/c_adjust
    #   } else if (rtmp[iter]>.6) {
    #     cc <- cc*c_adjust
    #   }
    # }

    # we are past the burn-in, save the draws
    if (iter > ndiscard) {
      s = iter - ndiscard
      postb[,s] = as.matrix(curr.beta)
      postg[,s] = as.matrix(curr.gamma)
      posts[s] = curr.sigma
      postr[s] = curr.rho

      # # calculate summary spatial effects
      # if (!is.null(origX)) {
      #   post.direct[,s] = curr.ai_diag/n * curr.beta[1:(smallk + 1)] +
      #                   c(0,curr.aiW_diag/n * curr.beta[(smallk + 2):smallk2] )
      #   post.total[,s] = curr.ai_tot/n * curr.beta[1:(smallk + 1)] +
      #                  c(0,curr.aiW_tot/n * curr.beta[(smallk + 2):smallk2] )
      # } else {
      #   post.direct[,s] = curr.ai_diag/n * curr.beta[1:(smallk + 1)]
      #   post.total[,s] = curr.ai_tot/n * curr.beta[1:(smallk + 1)]
      # }
      # post.indirect[,s] = post.total[,s] - post.direct[,s]
      #AI = solve(diag(n) - curr.rho * W)
      # for (rr in 1:k) {
      #   SW = AI %*% (diag(n) * curr.beta[rr])
      #   post.direct[rr,s] = sum(diag(SW))/n
      #   post.total[rr,s] = sum(SW)/n
      #   post.indirect[rr,s] = post.total[rr,s] - post.direct[rr,s]
      # }
    }
    setTxtProgressBar(pb,iter)
  }
  close(pb)
  return(list(postb = postb,postg = postg,posts = posts, postr=postr,
              post.direct = post.direct,post.indirect = post.indirect))
}




