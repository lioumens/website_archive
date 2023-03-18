#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  12-16-10                                                   #
# PURPOSE: Use binary regression models with the placekicking data  #
#                                                                   #
# NOTES:                                                            #
#####################################################################

placekick<-read.table(file = "C:\\data\\Placekick.csv", header = TRUE, sep = ",")
head(placekick)
tail(placekick)


#####################################################################
# Estimate the model

  mod.fit<-glm(formula = good ~ distance, family = binomial(link = logit), data = placekick)
  mod.fit 
  names(mod.fit)
  mod.fit$coefficients

  class(mod.fit)
  methods(class = glm)  # Method functions for object of class glm
  methods(class = lm)
  
  summary(object = mod.fit)  # summary(mod.fit) works too

  # Distance and change
  mod.fit2<-glm(formula = good ~ change + distance, family = binomial(link = logit), data = placekick)
  mod.fit2$coefficients

  summary(mod.fit2)


#####################################################################
# Estimated variances and covariances for the model

  # Show Coefficients table only
  temp<-summary(mod.fit)
  names(temp)
  temp$coefficients
  round(summary(mod.fit)$coefficients,4)  # Easier way to get the same result, I used round due to the width of the book
   
  # Estimated covariance matrix
  vcov(mod.fit)
  vcov(mod.fit)[2,2]  # Var^(beta^_1)
  summary(mod.fit)$coefficients[2,2]^2  # Var^(beta^_1)
  
  # Matrix calculations
  pi.hat<-mod.fit$fitted.values
  V<-diag(pi.hat*(1-pi.hat))
  V[1:3,1:3]
  X<-cbind(1, placekick$distance)  # Could also use model.matrix()
  solve(t(X) %*% V %*% X)
  
  # Part of the IRLS - this would be the next step if convergence had not already been obtained.
  Y<-placekick$good
  Z<-log(pi.hat/(1-pi.hat)) + diag(1/(pi.hat*(1-pi.hat)))%*%(Y-pi.hat) 
  solve(t(X)%*%V%*%X)%*%t(X)%*%V%*%Z   # One form
  mod.fit$coefficients + solve(t(X)%*%V%*%X)%*%t(X)%*%(Y-pi.hat)  # Another form

 
  ######################################
  # Code to show how optim() can be used to maximize log(L)
  
    # Log-likelihood function
    logL<-function(beta, x, Y) {
      pi<-exp(beta[1] + beta[2]*x)/(1+exp(beta[1] + beta[2]*x))
      # Alternatively, could use exp(X%*%beta)/(1+exp(X%*%beta)) where X is the design matrix
      sum( Y*log(pi) + (1-Y)*log(1-pi))
    }

    # Check value of the log-likelihood function at pi^
    logL(beta = mod.fit$coefficients, x = placekick$distance, Y = placekick$good)
    logLik(mod.fit)  # log-likelihood function at pi^

    # Find starting values for parameter estimates
    reg.mod<-lm(formula = good ~ distance, data = placekick)
    reg.mod$coefficients
    mod.fit.optim<-optim(par = reg.mod$coefficients, fn = logL, hessian = TRUE, x = placekick$distance, Y = placekick$good,
      control = list(fnscale = -1), method = "BFGS")
    # options(width = 70)  # Change display width for book
    names(mod.fit.optim)
    # options(width = 80)
    mod.fit.optim$par
    mod.fit.optim$value
    mod.fit.optim$convergence
    solve(mod.fit.optim$hessian)


  ######################################
  # 3D plot of the log-likelihood function
  
    # Evaluate the log-likelihood function at a lot of different values for beta0 and beta1
    beta0.values<-seq(from = -5, to = 18, by = 0.1)
    beta1.values<-seq(from = -0.65, to = 0.25, by = 0.01)
    count<-1
    save.logL<-numeric(length(beta0.values)*length(beta1.values))
    for (beta0 in beta0.values) {
      for (beta1 in beta1.values) {
        save.logL[count]<-logL(beta = c(beta0, beta1), x = placekick$distance, Y = placekick$good)
        count<-count+1
      }
    }
    max(save.logL)
 
    # Another way to evaluate the log-likelihood function
    # beta0.1.values<-expand.grid(beta0.values, beta1.values)   # Iterate out all possible beta0 and beta1 values (be careful about order)
    # save.logL1<--apply(X = beta0.1.values, MARGIN = 1, FUN = neglogL, x = placekick$distance, Y = placekick$good)
    # max(save.logL1)

 
    library(package = rgl)  # Package that does 3D interactive plots
    open3d()  # Open plot window
    # 3D plot with gridlines
    persp3d(x = beta1.values, y = beta0.values, z = save.logL, xlab = "beta1", ylab = "beta0", zlab = "log(L)", ticktype = "detailed", col="red")
    grid3d(c("x", "y+", "z"))
    
    
    # Put log-likelihood values with beta's in the correct form for contour plot
    save.logL2<-matrix(save.logL, nrow = length(beta0.values), ncol = length(beta1.values), byrow = T)
    save.logL[2]  # Verify correct order by checking one value
    save.logL2[1:2,1:2]
                
    # Contour plot
    x11(width = 7, height = 6, pointsize = 12)
    # pdf(file = "c:\\figures\\Figure2.3color.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
    par(pty = "s")
    contour(x = beta0.values, y = beta1.values, z = save.logL2,  xlab = 
      expression(beta[0]), ylab = expression(beta[1]), levels = -c(10000, 7500, 5000, 2500, 1000, 750, 500, 450, 400))
    # Grid lines are not at the major tick marks when drawn with panel.first=grid(col = "gray", lty="dotted") (not sure why),
    #  so draw them with abline()
    # abline(h = c(-0.6, -0.4, -0.2, 0, 0.2), col = "gray", lty="dotted")
    # abline(v = c(0, 5, 10, 15), col = "gray", lty="dotted")
    # MLEs
    abline(h = mod.fit$coefficients[2], lty = "dashed", lwd = 2, col= "red")
    abline(v = mod.fit$coefficients[1], lty = "dashed", lwd = 2, col= "red")
    # dev.off()  # Create plot for book

    # Show what a contour represents by finding log(L) approximately equalt to -500
    beta0.1.values<-expand.grid(beta1.values, beta0.values)
    save500<-data.frame(beta0.1.values[save.logL < -499 & save.logL > -501,], logL = save.logL[save.logL < -499 & save.logL > -501]) 
    points(x = save500$Var2, y = save500$Var1, pch = 20, col = "darkblue")
    
    # Black-and-white version of plot
    # pdf(file = "c:\\figures\\Figure2.3BW.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
    par(pty = "s")
    contour(x = beta0.values, y = beta1.values, z = save.logL2,  xlab =
      expression(beta[0]), ylab = expression(beta[1]), levels = -c(10000, 7500, 5000, 2500, 1000, 750, 500, 450, 400))
    # Grid lines are not at the major tick marks when drawn with panel.first=grid(col = "gray", lty="dotted") (not sure why),
    # so draw them with abline()
    # abline(h = c(-0.6, -0.4, -0.2, 0, 0.2), col = "gray", lty="dotted")
    # abline(v = c(0, 5, 10, 15), col = "gray", lty="dotted")
    # MLEs
    abline(h = mod.fit$coefficients[2], lty = "dashed", lwd = 2, col= "black")
    abline(v = mod.fit$coefficients[1], lty = "dashed", lwd = 2, col= "black")
    # dev.off()  # Create plot for book


#####################################################################
# Examine the binomial form of the data and re-fit the model

  # Find the observed proportion of successes at each distance
  w<-aggregate(formula = good ~ distance, data = placekick, FUN = sum)
  n<-aggregate(formula = good ~ distance, data = placekick, FUN = length)
  w.n<-data.frame(distance = w$distance, success = w$good, trials = n$good, proportion = round(w$good/n$good,4))
  head(w.n)
  tail(w.n)
  
  mod.fit.bin<-glm(formula = success/trials ~ distance, weights = trials, family = binomial(link = logit), data = w.n, trace = TRUE)
  summary(mod.fit.bin) 


#####################################################################
# Hypothesis tests

   library(package = car)  # Anova() is in the car package
   
   # Distance and change model again for completeness
   mod.fit2<-glm(formula = good ~ change + distance, family = binomial(link = logit), data = placekick)
   round(summary(mod.fit2)$coefficients, 4)   # Wald tests
   Anova(mod = mod.fit2, test.statistic="Wald")  # Another way to do Wald tests
   # One more way to do the Wald test
   Z0<-mod.fit2$coefficients[2]/sqrt(vcov(mod.fit2)[2,2])
   pvalue<-2*(1-pnorm(q = abs(Z0)))
   round(data.frame(Z0, pvalue),4)
  
  
   # LRTs
   Anova(mod.fit2, test = "LR")  # Given other variables in model, "test = 'LR' is the default
   anova(mod.fit2, test = "Chisq")  # Sequential testing of variables
   
   # Another way to perform the test given other variables are in the model
   drop1(object = mod.fit2, test = "LRT")
   library(package = lmtest)
   lrtest(mod.fit, mod.fit2)

   # Test Change from fits of two models
   mod.fit.Ho<-glm(formula = good ~ distance, family = binomial(link = logit), data = placekick)
   anova(mod.fit.Ho, mod.fit2, test = "Chisq")
   # Alternative way to perform the above test
   df<-mod.fit.Ho$df.residual-mod.fit2$df.residual
   stat<-mod.fit.Ho$deviance-mod.fit2$deviance
   pvalue<-1-pchisq(q = stat, df = df)
   data.frame(Ho.resid.dev = mod.fit.Ho$deviance, Ha.resid.dev = mod.fit2$deviance, df = df, 
     stat = round(stat,4), pvalue = round(pvalue,4))
     
 
   # Test change from fits of two models
   mod.fit.Ho<-glm(formula = good ~ distance, family = binomial(link = logit), data = placekick)
   anova(mod.fit.Ho, mod.fit2, test = "Chisq")


   # Test of Ho: logit(pi) = beta_0  vs. Ha: logit(pi) = beta_0 + beta_1*change
   mod.fit.Ho<-glm(formula = good ~ 1, family = binomial(link = logit), data = placekick)
   mod.fit.Ha<-glm(formula = good ~ change, family = binomial(link = logit), data = placekick)
   anova(mod.fit.Ho, mod.fit.Ha, test = "Chisq")
   pi.hat.Ho<-mod.fit.Ho$fitted.values
   pi.hat.Ha<-mod.fit.Ha$fitted.values
   y<-placekick$good
   stat<--2*sum(y*log(pi.hat.Ho/pi.hat.Ha) + (1-y)*log((1-pi.hat.Ho)/(1-pi.hat.Ha)))  # -2log(Lambda)
   pvalue<-1-pchisq(q = stat, df = 1)
   data.frame(stat, pvalue)
   head(pi.hat.Ho)  # All pi^'s same for Ho model
   mean(y)  # Observed proportion of successes
   

#####################################################################
# Saturated model, residual deviance, null deviance
 
   one.value<-1:nrow(placekick)  # Create one variable value per observation
   # Fit saturated model - the "-1" in the formula argument removes the intercept term from being estimated
   mod.fit.sat<-glm(formula = good ~ factor(one.value) - 1, family = binomial(link = logit), data = placekick)
   summary(mod.fit.sat)
   names(mod.fit.sat)
   mod.fit.sat$deviance
   mod.fit.sat$df.residual
   mod.fit.sat$converged
   mod.fit.sat$df.null
   mod.fit.sat$null.deviance


#####################################################################
# Odds ratios
  
  # Estimated odds ratio
  exp(mod.fit$coefficients[2])
  exp(-10*mod.fit$coefficients[2])
  
  # Wald interval
  beta.ci<-confint.default(object = mod.fit, parm = "distance", level = 0.95)
  beta.ci  # C.I. for beta
  exp(beta.ci)  # C.I. for OR with c = 1
  exp(beta.ci*10)  # C.I. for OR with c = 10
  rev(exp(-10*beta.ci))  # C.I. for OR with c = -10
  rev(1/exp(beta.ci*10))  # Invert C.I. for OR with c = 10, obviously the same as c = -10
  
  # Calculation without confint.default
  vcov(mod.fit)  # Var^(beta^_1) is in the (2,2) element of the matrix
  beta.ci<-mod.fit$coefficients[2] + qnorm(p = c(0.025, 0.975))*sqrt(vcov(mod.fit)[2,2])
  beta.ci
  rev(1/exp(beta.ci*10))
   
  # Profile likelihood interval
  beta.ci<-confint(object = mod.fit, parm = "distance", level = 0.95)
  beta.ci  # C.I. for beta
  rev(exp(-10*beta.ci))  # Invert C.I. for OR with c = 10, ignore limit labels
  as.numeric(rev(exp(-10*beta.ci)))  # Limit labels removed
 
  #More general way to obtain profile likelihood interval
  library(mcprofile)
  K <- matrix(data = c(0, 1), nrow = 1, ncol = 2, byrow = TRUE)
  linear.combo <- mcprofile(object = mod.fit, CM = K)
  ci.log.OR <- confint(object = linear.combo, level = 0.95, adjust = "none")
  names(ci.log.OR)
  as.numeric(rev(exp(-10*ci.log.OR$confint)))

  # method functions for confint()
  methods(confint)
  stats:::confint.glm  # uses MASS function
  MASS:::confint.glm  # confint.glm within MASS
  
  

#####################################################################
# Examples of how to find profile likelihood ratio intervals without confint()

  # Example of how to estimate the model logit(pi) = beta~_0 + beta_1*distance where beta_1*x is held constant and beta_1 = -0.12.
  #  The offset() function instructs R to not estimate a coefficient for beta1*x1 in the model (treat it as a constant). 
  #  Because there is only beta_0 remaining in the model, we need to use the "1" to tell R to estimate beta_0. 
  mod.fit.ex<-glm(formula = good ~ 1 + offset(-0.12*distance), family = binomial(link = logit), data = placekick)
  mod.fit.ex$coefficients
  logLik(mod.fit.ex)
  as.numeric(-2*(logLik(mod.fit.ex) - logLik(mod.fit)) - qchisq(p=0.95, df = 1))

  ########################
  # EXAMPLE using uniroot() to find the profile LR interval
  
    # Calculate -2log(Lambda) - 3.84
    find.root<-function(beta1, data.set, logLik.denom) {
      mod.fit.temp<-glm(formula = good ~ 1 + offset(beta1*distance), family = binomial(link = logit), data = data.set)
      as.numeric(-2*(logLik(mod.fit.temp)- logLik.denom) - qchisq(p=0.95, df = 1))
    }

    # Test
    find.root(beta1 = -0.12, data.set = placekick, logLik.denom = logLik(mod.fit))
    find.root(beta1 = -0.1318144, data.set = placekick, logLik.denom = logLik(mod.fit))  # Bound from confint()
    find.root(beta1 = -0.09907103, data.set = placekick, logLik.denom = logLik(mod.fit))  # Bound from confint()

    # Use uniroot
    save.lower<-uniroot(f = find.root, interval = c(-0.15, mod.fit$coefficients[2]), data.set = placekick, logLik.denom = logLik(mod.fit))
    save.upper<-uniroot(f = find.root, interval = c(mod.fit$coefficients[2], -0.05), data.set = placekick, logLik.denom = logLik(mod.fit))
    save.lower
    save.upper

    # OR interval
    round(1/c(exp(10*save.upper$root), exp(10*save.lower$root)), 4) 


  ########################
  # EXAMPLE using for loop to find the profile LR interval

    # Look for the interval over this set of possible values for beta1. Use Wald interval
    # for ideas of where to start looking for the lower and upper bounds.
    beta1<-seq(from = -0.15, to = -0.05, by = 0.0001)
  
    save.logLik<-matrix(data = NA, nrow = length(beta1), ncol = 1)  # Create a place to store log(L(beta~_0, beta_1))
    counter<-1
  
    # Iterate over possible values of beta1.
    for (beta1.profile in beta1) {
      mod.fit.temp<-glm(formula = good ~ 1 + offset(beta1.profile*distance), family = binomial(link = logit), data = placekick)
      save.logLik[counter]<-logLik(mod.fit.temp)
      counter<-counter+1
    }
   
    # -2log(Lambda) = -2[log(L(beta~_0, beta_1)) -  log(L(beta^_0, beta^_1)))
    save.check<--2*(save.logLik - logLik(mod.fit)) < qchisq(p=0.95, df = 1)
  
    # Interval for beta_1
    #  Examine only set of beta_1 values where -2log(Lambda) < 3.84. The profile interval
    #  bounds are the minimum and maximum of these values
    lower<-min(beta1[save.check])
    upper<-max(beta1[save.check])
    data.frame(lower, upper)  
  
    # OR interval
    round(c(1/exp(10*upper), 1/exp(10*lower)), 4) 
 

  
#####################################################################
# Estimating the probability of success

  # pi^
  linear.pred<-mod.fit$coefficients[1] + mod.fit$coefficients[2]*20
  linear.pred
  exp(linear.pred)/(1+exp(linear.pred))
  as.numeric(exp(linear.pred)/(1+exp(linear.pred)))  # Removes label
  plogis(q = linear.pred)  # This uses the inverse of the CDF of a logistic distribution
  
  predict.data<-data.frame(distance = 20)
  predict(object = mod.fit, newdata = predict.data, type = "link")
  predict(object = mod.fit, newdata = predict.data, type = "response")
  
  head(placekick$distance == 20)
  mod.fit$fitted.values[3]  # 3rd observation has distance = 20

  #########################
  # Wald interval
 
    alpha<-0.05
    linear.pred<-predict(object = mod.fit, newdata = predict.data, type = "link", se = TRUE)
    linear.pred
    pi.hat<-exp(linear.pred$fit)/(1+exp(linear.pred$fit))
    CI.lin.pred<-linear.pred$fit + qnorm(p = c(alpha/2, 1-alpha/2))*linear.pred$se
    CI.pi<-exp(CI.lin.pred)/(1+exp(CI.lin.pred))
    CI.pi
    round(data.frame(predict.data, pi.hat, lower = CI.pi[1], upper = CI.pi[2]),4)

  #########################
  # Additional examples for the Wald interval: 

    # Estimate pi at distance = 20 and 30 and change = 1
    predict.data<-data.frame(distance = c(20,30), change = c(1, 1))
    predict.data
    alpha<-0.05
    linear.pred<-predict(object = mod.fit2, newdata = predict.data, type = "link", se = TRUE)
    CI.lin.pred.x20<-linear.pred$fit[1] + qnorm(p = c(alpha/2, 1-alpha/2)) * linear.pred$se[1]
    CI.lin.pred.x30<-linear.pred$fit[2] + qnorm(p = c(alpha/2, 1-alpha/2)) * linear.pred$se[2]
    round(exp(CI.lin.pred.x20)/(1+exp(CI.lin.pred.x20)),4)  # CI for distance = 20
    round(exp(CI.lin.pred.x30)/(1+exp(CI.lin.pred.x30)),4)  # CI for distance = 30

    # Finding the Wald interval for pi for distance = 20 without predict()
    x<-20  # Distance = 20
    alpha<-0.05
    linear.pred<-mod.fit$coefficients[1] + mod.fit$coefficients[2]*x
    cov.mat<-vcov(mod.fit)  # Estimate covariance matrix
    var.linear.pred<-cov.mat[1,1] + x^2*cov.mat[2,2] + 2*x*cov.mat[1,2]  # Var^(beta^_0 + beta^_1 * x)
    sqrt(var.linear.pred)  # Matches linear.pred$se
    CI.lin.pred<-linear.pred + qnorm(p = c(alpha/2, 1-alpha/2))*sqrt(var.linear.pred)
    round(exp(CI.lin.pred)/(1+exp(CI.lin.pred)),4)
   
    # Confidence interval for pi using alterative formula that does not guarantee >=0 and <=1 for every x
    #  This method is NOT recommended for general use!
    predict.data<-data.frame(distance = 20)
    alpha<-0.05
    pi.hat<-predict(object = mod.fit, newdata = predict.data, type = "response", se = TRUE)
    pi.hat
    ci.pi2<-pi.hat$fit + qnorm(p = c(alpha/2, 1-alpha/2))*pi.hat$se
    data.frame(predict.data, pi.hat = round(pi.hat$fit,4), lower = round(ci.pi2[1],4), upper = round(ci.pi2[2],4))

  
  #########################
  # Profile LR intervals 
                           
    library(package = mcprofile)

    # Setting up matrix to get beta_0 + beta_1
    K<-matrix(data = c(1, 20), nrow = 1, ncol = 2)  
    K
    linear.combo<-mcprofile(object = mod.fit, CM = K)  # Calculate -2log(Lambda)
    ci.logit.profile<-confint(object = linear.combo, level = 0.95)  # CI for beta_0 + beta_1 * x
    ci.logit.profile
    names(ci.logit.profile)
    ci.logit.profile$confint
    exp(ci.logit.profile$confint)/(1 + exp(ci.logit.profile$confint))
    expit(ci.logit.profile)  # Function in package that is supposed to calculate the same quantity as above
    1/(1 + exp(-ci.logit.profile$confint))  # Another way to perform transformation

    names(ci.logit.profile)
    # exp(ci.logit.profile)/(1 + exp(ci.logit.profile))  # Does not work


  #########################
  # Additional examples for profile LR interval: 

    # C.I.s for pi at 20 and 30 yard placekicks
    K<-matrix(data = c(1, 20,
                       1, 30), nrow = 2, ncol = 2, byrow = TRUE)  # byrow = TRUE is NEEDED so that each row corresponds to ONE linear combination of betas
    K   
    linear.combo<-mcprofile(object = mod.fit, CM = K)
    ci.logit.profile<-confint(object = linear.combo, level = 0.95, adjust = "none")  # CI for beta_0 + beta_1 * x, adjust="none" is NEEDED to avoid adjustments to CIs that control familywise confidence levels
    ci.logit.profile
    exp(ci.logit.profile$confint)/(1 + exp(ci.logit.profile$confint))


    # Control familywise confidence level using Bonferroni adjustment
    ci.logit.profile<-confint(object = linear.combo, level = 0.95, adjust = "bonferroni")  # CI for beta_0 + beta_1 * x, adjust="none" is NEEDED to avoid adjustments to C.I.s to control familywise error rates
    ci.logit.profile
    exp(ci.logit.profile$confint)/(1 + exp(ci.logit.profile$confint))
          
    # CIs for pi at 20 and 30 yard placekicks with change equal to 1
    K<-matrix(data = c(1, 1, 20, 
                       1, 1, 30), nrow = 2, ncol = 3, byrow = TRUE)  # byrow = TRUE is NEEDED so that each row corresponds to ONE linear combination of betas
    K   
    linear.combo<-mcprofile(object = mod.fit2, CM = K)
    ci.logit.profile<-confint(object = linear.combo, level = 0.95, adjust = "none")  # CI for beta_0 + beta_1 * x, adjust="none" is NEEDED to avoid adjustments to C.I.s to control familywise error rates
    ci.logit.profile
    exp(ci.logit.profile$confint)/(1 + exp(ci.logit.profile$confint))
   
    # Wald intervals using wald() function
    save.wald<-wald(object = linear.combo)
    ci.logit.wald<-confint(object = save.wald, level = 0.95, adjust = "none")
    data.frame(lower = plogis(ci.logit.wald$confint[,1]), upper = plogis(ci.logit.wald$confint[,2]))
    # Note: plogis() is a quicker way to make the exp( )/( 1 + exp() ) transformation - see the generalized linear models section for more information
    

    # Earlier versions of mcprofile() allowed for the use of multiple cores. We left this code
    #  here in case the maintainer of the package reincorporate this functionality.
    # Example of using the mc.cores argument. This reduced my computation time by about 36%
    # start.time<-proc.time()    # Find start time

    # C.I.s for pi at 20 and 30 yard placekicks
    # K<-matrix(data = c(1, 20,
    #                   1, 30), nrow = 2, ncol = 2, byrow = TRUE) #byrow = TRUE is NEEDED so that each row corresponds to ONE linear combination of betas
    # K
    # linear.combo<-mcprofile(object = mod.fit, CM = K, mc.cores = 2)
    # ci.logit.profile<-confint(object = linear.combo, level = 0.95, adjust = "none") #CI for beta_0 + beta_1 * x, adjust="none" is NEEDED to avoid adjustments to CIs that control familywise confidence levels
    # ci.logit.profile
    # exp(ci.logit.profile$confint)/(1 + exp(ci.logit.profile$confint))

    # end.time<-proc.time()
    # save.time<-end.time-start.time
    # cat("\n Number of minutes running:", save.time[3]/60,  "\n \n")


#####################################################################
# Plots

  # Find the observed proportion of successes at each distance
  w<-aggregate(formula = good ~ distance, data = placekick, FUN = sum)
  n<-aggregate(formula = good ~ distance, data = placekick, FUN = length)
  w.n<-data.frame(distance = w$distance, success = w$good, trials = n$good, proportion = round(w$good/n$good,4))
  head(w.n)
  tail(w.n)
  
  # Plot of the observed proportions with logistic regression model
  x11(width = 7, height = 6, pointsize = 12)
  # pdf(file = "c:\\figures\\Figure2.4color.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
  plot(x = w$distance, y = w$good/n$good, xlab = "Distance (yards)", ylab = "Estimated probability",
       panel.first = grid(col = "gray", lty = "dotted"))
  # Put estimated logistic regression model on the plot
  curve(expr = predict(object = mod.fit, newdata = data.frame(distance = x), type = "response"), col = "red", add = TRUE,
    xlim = c(18, 66))


  ##########################
  # Add CI bounds to the plot
  
    # Function for C.I.s - need in order to use with curve function
    ci.pi<-function(newdata, mod.fit.obj, alpha){
      # print(newdata)  # Test
      linear.pred<-predict(object = mod.fit.obj, newdata = newdata, type = "link", se = TRUE)
      CI.lin.pred.lower<-linear.pred$fit - qnorm(p = 1-alpha/2)*linear.pred$se
      CI.lin.pred.upper<-linear.pred$fit + qnorm(p = 1-alpha/2)*linear.pred$se
      CI.pi.lower<-exp(CI.lin.pred.lower) / (1 + exp(CI.lin.pred.lower))
      CI.pi.upper<-exp(CI.lin.pred.upper) / (1 + exp(CI.lin.pred.upper))
      list(lower = CI.pi.lower, upper = CI.pi.upper)
    }
  
    # Test cases
    ci.pi(newdata = data.frame(distance = 20), mod.fit.obj = mod.fit, alpha = 0.05)
    ci.pi(newdata = data.frame(distance = 60), mod.fit.obj = mod.fit, alpha = 0.05)

    # Plot C.I. bands
    curve(expr = ci.pi(newdata = data.frame(distance = x), mod.fit.obj = mod.fit, alpha = 0.05)$lower, col = "blue", 
        lty = "dotdash", add = TRUE, xlim = c(18, 66))
    curve(expr = ci.pi(newdata = data.frame(distance = x), mod.fit.obj = mod.fit, alpha = 0.05)$upper, col = "blue", 
        lty = "dotdash", add = TRUE, xlim = c(18, 66))
      
    # Legend
    # legend(locator(1), legend = c("Logistic regression model", "95% individual C.I."), lty = c("solid", "dotdash"), col = c("red", "blue"), bty = "n")
    legend(x = 20, y = 0.4, legend = c("Logistic regression model", "95% individual C.I."), lty = c("solid", "dotdash"), col = c("red", "blue"), bty = "n")
    # dev.off()  # Create plot for book

 
    # Black-and-white version of plot
    # pdf(file = "c:\\figures\\Figure2.4BW.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
    plot(x = w$distance, y = w$good/n$good, xlab = "Distance (yards)", ylab = "Estimated probability")
    curve(expr = predict(object = mod.fit, newdata = data.frame(distance = x), type = "response"),
      col = "black", add = TRUE, xlim = c(18, 66))
    curve(expr = ci.pi(newdata = data.frame(distance = x), mod.fit.obj = mod.fit, alpha = 0.05)$lower, col = "black",
        lty = "dotdash", add = TRUE, xlim = c(18, 66))
    curve(expr = ci.pi(newdata = data.frame(distance = x), mod.fit.obj = mod.fit, alpha = 0.05)$upper, col = "black",
        lty = "dotdash", add = TRUE, xlim = c(18, 66))
    legend(x = 20, y = 0.4, legend = c("Logistic regression model", "95% individual C.I."),
      lty = c("solid", "dotdash"), col = c("black", "black"), bty = "n")
    # dev.off()  # Create plot for book




  ######################## 
  # Same as previous plot but now with plotting symbols proportional to n

    x11(width = 7, height = 6, pointsize = 12)
    # pdf(file = "c:\\figures\\Figure2.5color.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
    symbols(x = w$distance, y = w$good/n$good, circles = sqrt(n$good), inches = 0.5, xlab = "Distance (yards)", ylab = "Estimated probability",
         panel.first = grid(col = "gray", lty = "dotted"))
    # Put estimated logistic regression model on the plot
    curve(expr = predict(object = mod.fit, newdata = data.frame(distance = x), type = "response"), col = "red", add = TRUE,
      xlim = c(18, 66))
    # Plot C.I. bands
    curve(expr = ci.pi(newdata = data.frame(distance = x), mod.fit.obj = mod.fit, alpha = 0.05)$lower, col = "blue", 
        lty = "dotdash", add = TRUE, xlim = c(18, 66))
    curve(expr = ci.pi(newdata = data.frame(distance = x), mod.fit.obj = mod.fit, alpha = 0.05)$upper, col = "blue", 
        lty = "dotdash", add = TRUE, xlim = c(18, 66))
    # Legend
    # legend(locator(1), legend = c("Logistic regression model", "95% individual C.I."), lty = c("solid", "dotdash"), col = c("red", "blue"), bty = "n")
    legend(x = 20, y = 0.4, legend = c("Logistic regression model", "95% individual C.I."), lty = c("solid", "dotdash"), col = c("red", "blue"), bty = "n")
    # dev.off()  # Create plot for book

    # Black-and-white version of plot
    # pdf(file = "c:\\figures\\Figure2.5BW.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
    symbols(x = w$distance, y = w$good/n$good, circles = sqrt(n$good), inches = 0.5, xlab = "Distance (yards)", ylab = "Estimated probability")
    curve(expr = predict(object = mod.fit, newdata = data.frame(distance = x), type = "response"), col = "black", add = TRUE,
      xlim = c(18, 66))
    curve(expr = ci.pi(newdata = data.frame(distance = x), mod.fit.obj = mod.fit, alpha = 0.05)$lower, col = "black",
        lty = "dotdash", add = TRUE, xlim = c(18, 66))
    curve(expr = ci.pi(newdata = data.frame(distance = x), mod.fit.obj = mod.fit, alpha = 0.05)$upper, col = "black",
        lty = "dotdash", add = TRUE, xlim = c(18, 66))
    legend(x = 20, y = 0.4, legend = c("Logistic regression model", "95% individual C.I."), lty = c("solid", "dotdash"), col = c("black", "black"), bty = "n")
    # dev.off()  # Create plot for book


    head(w.n[rev(order(n$good)),]) # This shows that 789 observations are at distance of 20 yards
                                 # but the next largest is only 30, which is at a distance of 32 yards  
   
    # Possible points of concern?
    w.n.subset<-w.n[w.n$distance == 32 | w.n$distance == 43 | w.n$distance == 46 | w.n$distance == 51,]  # "|" means "or"
    predict.data<-data.frame(distance=c(32, 43, 46, 51))
    pi.hat<-predict(object = mod.fit, newdata = predict.data, type = "response")
    data.frame(w.n.subset, pi.hat)
  

  ########################
  # profile LR ratio version of the plot

    # Easiest way - add the bands to the previous plot
    #  Notice the bands are practically the same as those from the Wald intervals
    distances<-18:66
    K<-cbind(1, distances)
    class(K)  # matrix
    head(K)
    linear.combo<-mcprofile(object = mod.fit, CM = K)  # Calculate -2log(Lambda)
    ci.logit.profile<-confint(object = linear.combo, level = 0.95, adjust = "none")  # CI for beta_0 + beta_1 * x
    ci.logit.profile
    profile.lr.int<-exp(ci.logit.profile$confint)/(1 + exp(ci.logit.profile$confint))
    # Add bands
    lines(x = distances, y = profile.lr.int[,1], col = "green")
    lines(x = distances, y = profile.lr.int[,2], col = "green")


    # Another way to do the sample type of plot using a new ci.pi()-like function
    ci.pi2<-function(x, mod.fit.obj, alpha){
      K<-cbind(1, x)
      linear.combo<-mcprofile(object = mod.fit, CM = K)
      ci.logit.profile<-confint(object = linear.combo, level = 1 - alpha, adjust = "none")
      profile.lr.int<-exp(ci.logit.profile$confint)/(1 + exp(ci.logit.profile$confint))
      list(lower = profile.lr.int[,1], upper = profile.lr.int[,2])
    }

    # Test case
    ci.pi2(x = 20, mod.fit.obj = mod.fit, alpha = 0.05)

    x11(width = 7, height = 6, pointsize = 12)
    symbols(x = w$distance, y = w$good/n$good, circles = sqrt(n$good), inches = 0.5, xlab = "Distance (yards)", ylab = "Estimated probability",
         panel.first = grid(col = "gray", lty = "dotted"))
    # Put estimated logistic regression model on the plot
    curve(expr = predict(object = mod.fit, newdata = data.frame(distance = x), type = "response"), col = "red", add = TRUE,
      xlim = c(18, 66))
    # Plot C.I. bands
    curve(expr = ci.pi2(x = x, mod.fit.obj = mod.fit, alpha = 0.05)$lower, col = "blue",
        lty = "dotdash", add = TRUE, xlim = c(18, 66))
    curve(expr = ci.pi2(x = x, mod.fit.obj = mod.fit, alpha = 0.05)$upper, col = "blue",
        lty = "dotdash", add = TRUE, xlim = c(18, 66))
    # Legend
    legend(locator(1), legend = c("Logistic regression model", "95% individual C.I."), lty = c("solid", "dotdash"), col = c("red", "blue"), bty = "n")



  
#####################################################################
# Transformations
  
  mod.fit.Ho<-glm(formula = good ~ distance + wind, family = binomial(link = logit), data = placekick)
  mod.fit.Ha<-glm(formula = good ~ distance + wind + distance:wind, family = binomial(link = logit), data = placekick)
  summary(mod.fit.Ha)
  anova(mod.fit.Ho, mod.fit.Ha, test = "Chisq")
  library(package = car)  # Would need this if it has not already been used.
  Anova(mod.fit.Ha, test = "LR")  # Simpler way for the test
  Anova(mod.fit.Ho, test = "LR")  # Notice that the test statistics match the first two given in Anova(mod.fit.Ha, test = "LR")
    # This helps to demonstrate that the "distance:wind" interaction would not be present in the test for distance (or wind)
    



  #################################################
  # OR for wind at a specific distance

    beta.hat<-mod.fit.Ha$coefficients[2:4]  # Pull out beta^_1, beta^_2, beta^_3 so that we are using the same index as subscripts in the model (helpful to reduce coding errors)
                                            # Could also use  mod.fit.Ha$coefficients[-1]
    c<-1
    distance<-seq(from = 20, to = 60, by = 10)  # Examine distances 20 to 60 by 10 yard increments
    OR.wind<-exp(c*(beta.hat[2] + beta.hat[3]*distance))  # Estimated OR
    cov.mat<-vcov(mod.fit.Ha)[2:4,2:4]  # Pull out covariance matrix for beta^_1, beta^_2, beta^_3
    var.log.OR<-cov.mat[2,2] + distance^2*cov.mat[3,3] + 2*distance*cov.mat[2,3]   # Var(beta^_2 + distance*beta^_3)
    ci.log.OR.low<-c*(beta.hat[2] + beta.hat[3]*distance) - c*qnorm(p = 0.975)*sqrt(var.log.OR)  # Will not work correctly if use qnorm(p = c(0.025, 0.975)) due to multiple vectors being used
    ci.log.OR.up<-c*(beta.hat[2] + beta.hat[3]*distance) + c*qnorm(p = 0.975)*sqrt(var.log.OR)
    data.frame(OR.wind, OR.low = exp(ci.log.OR.low), OR.up = exp(ci.log.OR.up))
    round(data.frame(distance = distance, OR.hat = 1/OR.wind, OR.low = 1/exp(ci.log.OR.up), OR.up = 1/exp(ci.log.OR.low)),2)  #Inverted
  
  #################################################
  # OR for distance at a specific wind
  
    c<-10   # 10-yard increment
    wind<-0:1  # Examine wind for 0 and 1
    OR.dist<-exp(c*(beta.hat[1] + beta.hat[3]*wind))  # Estimated OR
    cov.mat<-vcov(mod.fit.Ha)[2:4,2:4]  # Pull out covariance matrix for beta^_1, beta^_2, beta^_3
    var.log.OR<-cov.mat[1,1] + wind^2*cov.mat[3,3] + 2*wind*cov.mat[1,3]   # Var(beta^_2 + distance*beta^_3)
    ci.log.OR.low<-c*(beta.hat[1] + beta.hat[3]*wind) - c*qnorm(p = 0.975)*sqrt(var.log.OR)  # Will not work correctly if use qnorm(p = c(0.025, 0.975)) due to multiple vectors being used
    ci.log.OR.up<-c*(beta.hat[1] + beta.hat[3]*wind) + c*qnorm(p = 0.975)*sqrt(var.log.OR)
    data.frame(OR.dist, OR.low = exp(ci.log.OR.low), OR.up = exp(ci.log.OR.up))
    round(data.frame(wind = wind, OR.hat = 1/OR.dist, OR.low = 1/exp(ci.log.OR.up), OR.up = 1/exp(ci.log.OR.low)),2)  #Inverted



  #################################################
  # Profile LR intervals using mcprofile package
  
    library(package = mcprofile)   # Need if had not already been used

    K<-matrix(data = c(0, 0, 1, 20,
                       0, 0, 1, 30,
                       0, 0, 1, 40,
                       0, 0, 1, 50,
                       0, 0, 1, 60), nrow = 5, ncol = 4, byrow = TRUE)
    # A little quicker way to form K
    # distance<-seq(from = 20, to = 60, by = 10)
    # K<-cbind(0, 0, 1, distance)

    K
    # profile LR
    linear.combo<-mcprofile(object = mod.fit.Ha, CM = K)
    ci.log.OR<-confint(object = linear.combo, level = 0.95, adjust = "none")
    # ci.log.OR
    data.frame(distance, OR.hat = round(1/exp(ci.log.OR$estimate), 2),
      OR.low = round(1/exp(ci.log.OR$confint$upper), 2),
      OR.up = round(1/exp(ci.log.OR$confint$lower), 2))

    # Wald
    save.wald<-wald(object = linear.combo)
    ci.log.OR.wald<-confint(object = save.wald, level = 0.95, adjust = "none")
    data.frame(distance, OR.hat = round(1/exp(ci.log.OR.wald$estimate), 2),
      OR.low = round(1/exp(ci.log.OR.wald$confint$upper), 2),
      OR.up = round(1/exp(ci.log.OR.wald$confint$lower), 2))


    # OR for 10 yard decrease in distance holding wind constant at 0 or 1
    K<-matrix(data = c(0, -10, 0, 0,
                       0, -10, 0, -10), nrow = 2, ncol = 4, byrow = TRUE)
    K
    # profile LR
    linear.combo<-mcprofile(object = mod.fit.Ha, CM = K)
    ci.log.OR<-confint(object = linear.combo, level = 0.95, adjust = "none")
    exp(ci.log.OR)

    # Wald
    save.wald<-wald(object = linear.combo)
    ci.log.OR.wald<-confint(object = save.wald, level = 0.95, adjust = "none")
    exp(ci.log.OR.wald)



  #################################################
  # Equivalent ways to estimate good ~ distance + wind + distance:wind

    mod.fit.dw2<-glm(formula = good ~ distance*wind, family = binomial(link = logit), data = placekick)
    summary(mod.fit.dw2)

    mod.fit.dw3<-glm(formula = good ~ (distance + wind)^2, family = binomial(link = logit), data = placekick)
    summary(mod.fit.dw3)
  
  
  
  #################################################
  # Plot
 
    x11(width = 10, height = 6, pointsize = 12)
    # pdf(file = "c:\\figures\\Figure2.6color.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
    par(mfrow = c(1,2))

    curve(expr = predict(object = mod.fit.Ho, newdata = data.frame(distance = x, wind = 0), type = "response"), col = "red", lty = "solid", xlim = c(20,60),
          ylim = c(0,1), ylab = "Estimated probability", main = "Without interaction",
          xlab = "Distance", panel.first = grid(col = "gray", lty = "dotted"), cex.main = 0.9, lwd = 1)
    curve(expr = predict(object = mod.fit.Ho, newdata = data.frame(distance = x, wind = 1), type = "response"),
      col = "blue", lty = "dotdash", lwd = 1, add = TRUE)
    legend(x = 20, y = 0.4, legend = c("Wind = 0", "Wind = 1"), lty = c("solid", "dotdash"), col = c("red", "blue"),
      lwd = c(1,1), bty = "n")

    curve(expr = predict(object = mod.fit.Ha, newdata = data.frame(distance = x, wind = 0), type = "response"), col = "red", lty = "solid", xlim = c(20,60),
          ylim = c(0,1), ylab = "Estimated probability", main = "With interaction",
          xlab = "Distance", panel.first = grid(col = "gray", lty = "dotted"), cex.main = 0.9, lwd = 1)
    curve(expr = predict(object = mod.fit.Ha, newdata = data.frame(distance = x, wind = 1), type = "response"),
      col = "blue", lty = "dotdash", lwd = 1, add = TRUE)
    legend(x = 20, y = 0.4, legend = c("Wind = 0", "Wind = 1"), lty = c("solid", "dotdash"), col = c("red", "blue"),
      lwd = c(1,1), bty = "n")
    # Old plot title: expression(logit(hat(pi)) == hat(beta)[0] + hat(beta)[1]*distance + hat(beta)[2]*wind + hat(beta)[3]*distance%*%wind)
    # dev.off()  # Create plot for book

    # Black-and-white version of plot
    # pdf(file = "c:\\figures\\Figure2.6BW.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
    par(mfrow = c(1,2))
    curve(expr = predict(object = mod.fit.Ho, newdata = data.frame(distance = x, wind = 0), type = "response"), col = "black", lty = "solid", xlim = c(20,60),
          ylim = c(0,1), ylab = "Estimated probability", main = "Without interaction",
          xlab = "Distance", cex.main = 0.9, lwd = 1)
    curve(expr = predict(object = mod.fit.Ho, newdata = data.frame(distance = x, wind = 1), type = "response"),
      col = "black", lty = "dotdash", lwd = 1, add = TRUE)
    legend(x = 20, y = 0.4, legend = c("Wind = 0", "Wind = 1"), lty = c("solid", "dotdash"), col = c("black", "black"),
      lwd = c(1,1), bty = "n")

    curve(expr = predict(object = mod.fit.Ha, newdata = data.frame(distance = x, wind = 0), type = "response"), col = "black", lty = "solid", xlim = c(20,60),
          ylim = c(0,1), ylab = "Estimated probability", main = "With interaction",
          xlab = "Distance", cex.main = 0.9, lwd = 1)
    curve(expr = predict(object = mod.fit.Ha, newdata = data.frame(distance = x, wind = 1), type = "response"),
      col = "black", lty = "dotdash", lwd = 1, add = TRUE)
    legend(x = 20, y = 0.4, legend = c("Wind = 0", "Wind = 1"), lty = c("solid", "dotdash"), col = c("black", "black"),
      lwd = c(1,1), bty = "n")
    # dev.off()  # Create plot for book

   
#####################################################################
# Estimate the model using the convergence arguments

  mod.fit<-glm(formula = good ~ distance, family = binomial(link = logit), data = placekick, trace = TRUE, epsilon = 0.0001, maxit = 50)
  # names(mod.fit)
  mod.fit$control
  mod.fit$converged
  mod.fit$coefficients
  
  # Check convergence
  abs(775.745-775.7451)/(0.1 + abs(775.745))
  abs(775.7451-775.8357)/(0.1 + abs(775.7451))
  
  
  # 3 iterations only to show non-convergence
  mod.fit.noconv<-glm(formula = good ~ distance, family = binomial(link = logit), data = placekick, trace = TRUE, epsilon = 0.0001, maxit = 3)
  mod.fit.noconv$coefficients

  


#####################################################################
# Estimate the logistic, probit, and complementary log-log models

  # Logistic
  mod.fit.logit<-glm(formula = good ~ distance, family = binomial(link = logit), data = placekick)
  round(summary(mod.fit.logit)$coefficients, 4)
  
  # Probit
  mod.fit.probit<-glm(formula = good ~ distance, family = binomial(link = probit), data = placekick)
  round(summary(mod.fit.probit)$coefficients, 4)

  # Complementary log-log
  mod.fit.cloglog<-glm(formula = good ~ distance, family = binomial(link = cloglog), data = placekick)
  round(summary(mod.fit.cloglog)$coefficients, 4)
  
  # Compare pi^ values without predict()
  distance<-c(20, 35, 50)
  plogis(q = mod.fit.logit$coefficients[1] + mod.fit.logit$coefficients[2]*distance)  # Logistic
  pnorm(q = mod.fit.probit$coefficients[1] + mod.fit.probit$coefficients[2]*distance)  # Probit
  1-exp(-exp(mod.fit.cloglog$coefficients[1] + mod.fit.cloglog$coefficients[2]*distance))  # Complementary log-log
  
  # Compare pi^ values with predict() (easier)
  predict.data<-data.frame(distance = c(20, 35, 50))
  logistic.pi<-predict(object = mod.fit.logit, newdata = predict.data, type = "response")
  probit.pi<-predict(object = mod.fit.probit, newdata = predict.data, type = "response")
  cloglog.pi<-predict(object = mod.fit.cloglog, newdata = predict.data, type = "response")
  round(data.frame(predict.data, logistic.pi, probit.pi, cloglog.pi),4)    

  # For distance = 20 example and probit model
  lin.pred<-as.numeric(mod.fit.probit$coefficients[1] + mod.fit.probit$coefficients[2]*20)
  lin.pred
  pnorm(q = lin.pred)
  pnorm(q = 1.951195)

  
  ###################
  # Bubble plot - Much of the same code from before is included here
  w<-aggregate(formula = good ~ distance, data = placekick, FUN = sum)
  n<-aggregate(formula = good ~ distance, data = placekick, FUN = length)
  w.n<-data.frame(distance = w$distance, success = w$good, trials = n$good, proportion = round(w$good/n$good,4))
   
  # Plot of the observed proportions with logistic regression model
  x11(width = 7, height = 6, pointsize = 12)
  # pdf(file = "c:\\figures\\Figure2.14color.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
  symbols(x = w$distance, y = w$good/n$good, circles = sqrt(n$good), inches = 0.5, xlab="Distance (yards)", ylab="Estimated probability",
     panel.first = grid(col = "gray", lty = "dotted"))
  # Estimated logistic regression model
  curve(expr = predict(object = mod.fit.logit, newdata = data.frame(distance = x), type = "response"),
    col = "red", lwd = 2, add = TRUE, lty = 1, xlim = c(18,66))
  # Estimated probit model
  curve(expr = predict(object = mod.fit.probit, newdata = data.frame(distance = x), type = "response"),
    col = "blue", lwd = 2, add = TRUE, lty = 2, xlim = c(18,66))
  # Estimated complementary log-log model
  curve(expr = predict(object = mod.fit.cloglog, newdata = data.frame(distance = x), type = "response"),
    col = "green", lwd = 2, add = TRUE, lty = 4, xlim = c(18,66))

  # Legend
  legend(x = 18, y = 0.42, legend = c("Logistic", "Probit", "Complementary log-log"), lty = c(1, 2, 4), lwd = c(2,2,2),
     bty = "n", col=c("red", "blue", "green"), cex = 1)
  # dev.off()  # Create plot for book


  # Black-and-white version of plot
  # pdf(file = "c:\\figures\\Figure2.14BW.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
  symbols(x = w$distance, y = w$good/n$good, circles = sqrt(n$good), inches = 0.5, xlab="Distance (yards)", ylab="Estimated probability")
  curve(expr = predict(object = mod.fit.logit, newdata = data.frame(distance = x), type = "response"),
    col = "black", lwd = 2, add = TRUE, lty = 1, xlim = c(18,66))
  curve(expr = predict(object = mod.fit.probit, newdata = data.frame(distance = x), type = "response"),
    col = "black", lwd = 2, add = TRUE, lty = 2, xlim = c(18,66))
  curve(expr = predict(object = mod.fit.cloglog, newdata = data.frame(distance = x), type = "response"),
    col = "black", lwd = 2, add = TRUE, lty = 4, xlim = c(18,66))
  legend(x = 18, y = 0.42, legend = c("Logistic", "Probit", "Complementary log-log"), lty = c(1, 2, 4), lwd = c(2,2,2),
     bty = "n", col=c("black", "black", "black"), cex = 1)
  # dev.off()  # Create plot for book




  ###################
  # ORs
   
  pi.hat<-data.frame(predict.data, logistic.pi, probit.pi, cloglog.pi) 
  odds.x20<-pi.hat[1, 2:4]/(1 - pi.hat[1, 2:4])
  odds.x35<-pi.hat[2, 2:4]/(1 - pi.hat[2, 2:4])
  odds.x50<-pi.hat[3, 2:4]/(1 - pi.hat[3, 2:4])
         
  OR.20.35<-odds.x20/odds.x35
  OR.35.50<-odds.x35/odds.x50

  data.frame(OR = c("20 vs. 35", "35 vs. 50"), round(rbind(OR.20.35, OR.35.50),2) )


#