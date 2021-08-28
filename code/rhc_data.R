rhc = read.csv("data/rhc.csv", header = TRUE, sep = ",")
head(rhc)

## a list of covariates we will use 
xvars <- c("ARF", "CHF", "Cirr", "colcan", "Coma", "lungcan",
           "MOSF", "sepsis", "age", "female")


######## step 1 ###########
library(tableone)
table1 <- CreateTableOne(vars = xvars, strata = "treatment", data = rhc)
## include standardized mean difference (SMD)
print(table1, smd = TRUE)

library(cobalt)
bal.tab(treatment ~ ARF + CHF + Cirr + colcan + Coma + lungcan +
        MOSF + sepsis + age + female, data = rhc, estimand = "ATE")


######## steps 2-3 ###########
## full matching
library(MatchIt)
m.out <- matchit(treatment ~ ARF + CHF + Cirr + colcan + Coma + lungcan +
                   MOSF + sepsis + age + female, data = rhc,
                 method = "full", estimand = ATE) # it may take time..
print(summary(m.out, standardize = TRUE))

## weighting
library(WeightIt)
w.out <- weightit(treatment ~ ARF + CHF + Cirr + colcan + Coma + lungcan +
                    MOSF + sepsis + age + female, data = rhc, estimand = "ATE")
summary(w.out)
  

######## step 4 ###########
bal.tab(m.out, stats = "m", thresholds = c(m = 0.1))
love.plot(m.out, binary = "std", thresholds = c(m = 0.1))

bal.tab(w.out, stats = "m", thresholds = c(m = 0.1))
love.plot(w.out, binary = "std", thresholds = c(m = 0.1))

######## step 5 ###########
fit.m <- glm(died ~ treatment +ARF + CHF + Cirr + colcan + Coma + lungcan +
               MOSF + sepsis + age + female,
            data = match.data(m.out), family = binomial())
summary(fit.m)
confint(fit.m)[2,]

library(survey)
design.w <- svydesign(~1, weights = w.out$weights, data = rhc)
fit.w <- svyglm(died ~ treatment +ARF + CHF + Cirr + colcan + Coma + lungcan +
                  MOSF + sepsis + age + female,
                design = design.w, family = binomial())
## the target estimand w.out was ATE
confint(fit.w)[2,]

