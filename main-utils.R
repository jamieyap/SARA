# About File: -----------------------------------------------------------------
# main-utils.R contains functions to perform data analysis tasks

ImputeInterventionAssignment <- function(dataforanalysis.aimX, use.this.seed = 54239212){
  # ImputeInterventionAssignment() performs one imputation only for 
  # participant days which are available using use.this.seed
  
  set.seed(use.this.seed)
  tmp.miss.aimX <- dataforanalysis.aimX[is.na(dataforanalysis.aimX$isRandomized) & dataforanalysis.aimX$availability==1,]
  tmp.complete.aimX <- dataforanalysis.aimX[!(is.na(dataforanalysis.aimX$isRandomized) & dataforanalysis.aimX$availability==1),]
  tmp.miss.aimX$isRandomized <- sample(x = c(0,1), size = dim(tmp.miss.aimX)[1], replace = TRUE, prob = c(0.5, 0.5))
  dataforanalysis.aimX <- rbind(tmp.miss.aimX, tmp.complete.aimX)
  dataforanalysis.aimX <- dataforanalysis.aimX[order(dataforanalysis.aimX$username, dataforanalysis.aimX$study_day),]
  
  return(dataforanalysis.aimX)
}

PoolEstimatesMI <- function(inputs.beta, inputs.stderr, n, p, q, m){
  # inputs.beta = estimates of beta and alpha from each imputed dataset; this is of length m
  # inputs.stderr = estimates of the standard error of betahat and alphahat from each imputed dataset; this is of length m
  # n = number of participants
  # p = number of beta parameters (including intercept)
  # q = number of alpha parameters (including intercept)
  # m = number of imputed datasets
  
  pooled.beta <- rowMeans(inputs.beta)
  W <- rowMeans(inputs.stderr^2)
  B <- (1/(m-1))*rowSums((inputs.beta - pooled.beta)^2)
  pooled.var.beta <- W + (1 + 1/m)*B
  pooled.se.beta <- sqrt(pooled.var.beta)
  test.stat <- pooled.beta/pooled.se.beta
  p.val <- 2 * pt(abs(test.stat), df = n - p - q, lower.tail = FALSE)
  
  pooled.exp.beta <- rowMeans(exp(inputs.beta))
  pooled.results <- list(pooled.exp.beta=pooled.exp.beta,
                         pooled.beta=pooled.beta, 
                         pooled.se.beta=pooled.se.beta, 
                         test.stat=test.stat, 
                         p.val=p.val)
  pooled.results <- lapply(pooled.results, round, digits=3)
  
  return(pooled.results)
}

CheckQualityOfRand <- function(dataforanalysis.aimX){
  # CheckQualityOfRand() checks quality of randomization by calculating
  # the average (across days)
  #   - empirical probability of being offered an intervention
  #   - balance score of appusage_yes
  #   - balance score of isCompleted_yesterday_yes"
  #   - balance score of contact_yes
  
  empiricalprob.assignment.aimX <- aggregate(isRandomized ~ study_day, 
                                             data = dataforanalysis.aimX[dataforanalysis.aimX$availability==1,], 
                                             mean, na.rm = TRUE)
  
  balance.appusage.aimX <- aggregate(appusage_yes ~ study_day + isRandomized, 
                                     data = dataforanalysis.aimX[dataforanalysis.aimX$availability==1,], 
                                     mean, na.rm = TRUE)
  score.balance.appusage.aimX <- balance.appusage.aimX[balance.appusage.aimX$isRandomized==1,"appusage_yes"] - balance.appusage.aimX[balance.appusage.aimX$isRandomized==0,"appusage_yes"]
  
  balance.prevday.outcome.aimX <- aggregate(isCompleted_yesterday_yes ~ study_day + isRandomized, 
                                            data = dataforanalysis.aimX[dataforanalysis.aimX$availability==1,], 
                                            mean, na.rm = TRUE)
  score.balance.prevday.outcome.aimX <- balance.prevday.outcome.aimX[balance.prevday.outcome.aimX$isRandomized==1,"isCompleted_yesterday_yes"] - balance.prevday.outcome.aimX[balance.prevday.outcome.aimX$isRandomized==0,"isCompleted_yesterday_yes"]
  
  
  balance.contact_yes.aimX <- aggregate(contact_yes ~ study_day + isRandomized, 
                                        data = dataforanalysis.aimX[dataforanalysis.aimX$availability==1,], 
                                        mean, na.rm = TRUE)
  score.balance.contact_yes.aimX <- balance.contact_yes.aimX[balance.contact_yes.aimX$isRandomized==1,"contact_yes"] - balance.contact_yes.aimX[balance.contact_yes.aimX$isRandomized==0,"contact_yes"]
  
  check.rand.quality <- data.frame(aimX = c(mean(empiricalprob.assignment.aimX[,2]),
                                            mean(score.balance.appusage.aimX),
                                            mean(score.balance.prevday.outcome.aimX),
                                            mean(score.balance.contact_yes.aimX)))
  check.rand.quality <- round(check.rand.quality, digits = 2)
  row.names(check.rand.quality) <- c("Average: empirical probability of being offered an intervention", 
                                      "Average: balance score of appusage_yes", 
                                      "Average: balance score of isCompleted_yesterday_yes",
                                      "Average: balance score of contact_yes")
  
  return(list(empiricalprob.assignment.aimX = empiricalprob.assignment.aimX,
              score.balance.appusage.aimX = score.balance.appusage.aimX,
              score.balance.prevday.outcome.aimX = score.balance.prevday.outcome.aimX,
              score.balance.contact_yes.aimX = score.balance.contact_yes.aimX,
              check.rand.quality = check.rand.quality)
         )
}
