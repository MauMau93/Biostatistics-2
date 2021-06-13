## Exercise 1

# estimate survival function
# we have piecewise hazard function of h(t) = 0.07 when t <= 5 and h(t) = 0.4 when t > 5
# this implies that the survival function is piecewise exponentially distributed, 
# since we have a hazard function of the form 1/$\theta_{t}$
# where $\theta_{t}$ = 14.29 when t <= 5 and $\theta_{t}$ = 2.5 when t > 5


# build survival function
hazard_1 <- 14.29
hazard_2 <- 2.5
t_1 = seq(0,5, length.out = 100)
t_2 = seq(5.0001,10, length.out = 100)
t <- c(t_1, t_2)
hazard <- c(rep(hazard_1, 100), rep(hazard_2, 100))
survival_1 <- (1/hazard_1)*exp(-t_1/hazard_1)
survival_2 <- (1/hazard_2)*exp(-t_2/hazard_2)
survival <- c(survival_1, survival_2)
plot(t, hazard, ylim = c(0, 15), main = 'Piecewise hazard function')
plot(t, survival, main = 'Piecewise survival function')

# sample from survival distribution
x_1 <- rexp(1000, rate = 1/hazard_1)
x_2 <- rexp(1000, rate = 1/hazard_2)
x <- c(x_1, x_2)
hist(x, breaks = "FD", main = 'Histogram of Survival Function sample')
plot(density(x), xlim = c(0, 100), main = "Density estimate of Survival Function")

# median survival time 
median(survival)

## Exercise 4

data <- read.table("Henning.txt",header = TRUE)
head(data)
summary(data)

#First, we transform some variables into factors.

data$personal <- as.factor(data$personal)
data$property <- as.factor (data$property)
summary(data)
head(data)

# Let´s definr the censor status as is conventional: "0" if it is censored data and "1" if not censored.

data$censor = ifelse(data$censor == 1,0,1)
head(data$censor)
data <- data[,2:6]

# a) Compute and graph the Kaplan-Meier estimate of the survival function for all of the data
library(survival)
Surv(data$months,data$censor)[1:10]

# Let´s estimate the survival function:

fit <- survfit(Surv(months,censor)~1,data=data)
summary(fit)

# Let´s plot the survival function estimatation:

plot(fit,xlab ="Time",ylab = "Survival")

# Part b)

# Now, we need to divide the data in two groups: one composed of prisoners that were arrested for attacking a person, and others that were involved in other kind of crimes.

data_person <- data[data$personal==1,]
data_noperson <- data[data$personal==0,]

fit_person <- survfit(Surv(months,censor)~1,data=data_person)
summary(fit_person)
fit_noperson <- survfit(Surv(months,censor)~1,data=data_noperson)
summary(fit_noperson)

plot(fit_person,xlab ="Time",ylab = "Survival",main="Personal Attacks")
plot(fit_noperson,xlab ="Time",ylab = "Survival",main="Non Personal Attacks")

# We can plot both of them together now:

fit = survfit(Surv(months,censor )~ personal,data = data)
plot(fit, lty =1:2 ,col =1:2,xlab ="Time ",ylab ="Survival")
legend (c (75 ,150) , c (0.8,1),legend = c ( 0 ,1 ) , lty =1:2 , col =1:2)

# We can plot it with a nicer layout to observe better the differences:

library(ggfortify)
autoplot(fit)

# Now, we can perform a low-rank test to check if the two groups have the same distribution or not. 
# In other words, the null hypothesis that we will try to gather evidence against, in this case, is that both groups are the same.
# We can perform this test because we are studying a categorical variable now, either the inmate commited a felony related to attacking another person ("1), or did not ("0").

# low - rank test
survdiff(Surv(months,censor)~ personal,data = data)

# In this case, we get a very small p-value. So we can say that at a confindence level of 95% we reject the null hypothesis that claims that both groups are the same.
# In other words, there seems to be a statistically significant difference between the two Kaplan-Meier plots for each group. 
# Nevertheless, the result is only coherent when taking a significance level equal to 0.05. If we take, for instance, a significance level equal to 0.01, we can not reject any longer the null hypothesis, and at that confidence level (99%) we could not state that there is enough evidence to say that the two groups are not the same. 

# Part c)
# Now, we have to the the same procedure, but between two different groups now: the inmates that had commited a felony against a property, and the ones that did not have.
# So first we need to create two different groups:

data_property <- data[data$property==1,]
data_noproperty <- data[data$property==0,]

# Now, we can first fit a survival function for each group, and check them individualy.

fit_property <- survfit(Surv(months,censor)~1,data=data_property)
summary(fit_property)
fit_noproperty <- survfit(Surv(months,censor)~1,data=data_noproperty)
summary(fit_noproperty)

# We can also plot them individually, as follows:

plot(fit_property,xlab ="Time",ylab = "Survival",main="Property Felony")
plot(fit_noproperty,xlab ="Time",ylab = "Survival",main="No Property Felony")

# We can also fit a survival function for each group and plot them together:

fit_prop = survfit(Surv(months,censor )~ property,data = data)
plot(fit_prop, lty =1:2 ,col =1:2,xlab ="Time ",ylab ="Survival")
legend (c (75 ,150) , c (0.8,1),legend = c ( "0" ,"1" ) , lty =1:2 , col =1:2)

# We can plot it with a nicer layout to observe better the differences:
autoplot(fit_prop)

# As we can see in the plots, the two survival curves behave very differently, so we expect not to reject the null hypothesis of the low-rank test, that claims that both groups are the same.
# Let´s run the low-rank test and see what happens:
survdiff(Surv(months,censor)~ property,data = data)

# As we can see, the p-value of the test is very low, confirming our suspision that the groups differ in their survival functions.

# Part d)
# Now, we need to fit a Cox regression of time-to-arrest, using the covariates: personal, property and cage.
# We already have transformed some variables into factors when needed.

# Let´s fit this model first, then:

fit.cov = coxph(Surv(months,censor ) ~ personal + property + cage, data=data)

library(car)
Anova(fit.cov)

basehaz(fit.cov)
confint(fit.cov)
# As we can see the three variables seem to be significant in determining the survival of the inmates.

# As we can see, "cage" is the only variable whose confidence interval contains zero, meaning that is the only predictor that is not statisticaly significant.
# Both "personal" and "property" have their confidence intervals close to zero, but none of them include zero, at least at 95% confidence level. So when considering a significance level of 0.05, these predictors are significant.

# We can determine through a Wald Test which of the estimated coefficients are statisticaly significant:
summary(fit.cov)

# When calling "summary" and the name of the model with our covariates, we can observe in the column named "z" the the Wald statistic value.
# It corresponds to the ratio of each regression coefficient to its standard error (z = coef/se(coef)). The wald statistic evaluates, whether the beta (β) coefficient of a given variable is statistically significantly different from 0.
# As we can see in the output above, the three coefficients are significant (together), specially the one associated to the "cage" predictor. But we are not testing each variable in a separate way, but we are testing if  all the model is significant or not.
# Let´s apply the univariate "coxph" function tu multiple covariance at once:


covariates <- c("personal", "property",  "cage")
univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(months,censor)~', x)))

univ_models <- lapply( univ_formulas, function(x){coxph(x, data = data)})
# Extract data 
univ_results <- lapply(univ_models,
                       function(x){ 
                         x <- summary(x)
                         p.value<-signif(x$wald["pvalue"], digits=2)
                         wald.test<-signif(x$wald["test"], digits=2)
                         beta<-signif(x$coef[1], digits=2);#coeficient beta
                         HR <-signif(x$coef[2], digits=2);#exp(beta)
                         HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                         HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
                         HR <- paste0(HR, " (", 
                                      HR.confint.lower, "-", HR.confint.upper, ")")
                         res<-c(beta, HR, wald.test, p.value)
                         names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
                                       "p.value")
                         return(res)
                         #return(exp(cbind(coef(x),confint(x))))
                       })
res <- t(as.data.frame(univ_results, check.names = FALSE))
as.data.frame(res)

# As we can see here, when performing a cox function for each covariate, we see that we still claim that the three variables are significant.

# Coefficient Interpretation:
# It is easier to interpret the hazard ratios than the coefficient themselves. They give the effect size of covariates.

# i) Personal: 0.56914 (ratio hazard = 1.766). This means that being part of the group of inmates that did  commit a personal felony (against another person) increases the hazard by a factor of 1.766. Thus,having commited a felony that involves another person is bad for the prognostic.   
# ii) Property: 0.93579 (ratio hazard = 2.54922). This means that being part of the group of people who commited a property felony increases the hazard by  a factor of 2.549. Thus, having commited a felony regarding properties is bad for the prognostic.
# iii) Cage: -0.0667 (hazard ratio = 0.9354). This means that a one unit change in "cage" decreases the hazard of recomitting a felony by around 7%.

# We can hav a glance at all the covariates using the ggforest plot:

library(survminer)
ggforest (fit.cov, data = data)


# This plot shows the hazard ratios (HR) which are derived from the model for all covariates.
# According to this plot, having commited a felony regarding an attack to other people or to someone elses property will increase the hazard of recomitting a felony.
# In the case of "cage", it seems that the older the inmate gets, the lower is the hazard of recomitting a felony.



