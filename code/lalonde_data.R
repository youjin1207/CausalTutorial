library(Matching)
data(lalonde)
head(lalonde)

######## step 1 ###########
library(tableone)
xvars = colnames(lalonde)[!(colnames(lalonde) %in% c("treat", "re78"))]
table1 <- CreateTableOne(vars = xvars, strata = "treat", 
                         data = lalonde, test = FALSE)
print(table1, smd = TRUE)

library(cobalt) 
bal.tab(treat ~ age + educ + black + hisp +
          married + nodegr + re74 + re75 + u74 + u75,
        data = lalonde, estimand = "ATE", m.threshold = 0.05)


######## steps 2-5: subclassification ###########
library(MatchIt)
m.out.subclass <- matchit(treat ~ age + educ + hisp + 
                            married + nodegr + re75 + u74 + u75,
                          data = lalonde, method = "subclass")
print(summary(m.out.subclass, standardize = TRUE))

# save the matched data with subclass indicators
data.subcl <- match.data(m.out.subclass)
table(data.subcl$subclass)
n = nrow(data.subcl)
# generate subclass-specific effects, variances, and size
effects.subcl = vars.subcl = n.subcl = c()
# run regression model within each subclass
for(s in 1:6){
  fit.subcl <- lm(re78 ~ treat + age + educ + hisp + 
                    married + nodegr + re75 + u74 + u75,
                  data = data.subcl, subset = (subclass==s))
  effects.subcl[s] <- fit.subcl$coefficients[2]
  vars.subcl[s] <- (summary(fit.subcl)$coefficients[2,2])^2
  n.subcl[s] <- sum(data.subcl$subclass==s)
}

# derive overall average treatment effects via the weighted subclass effects
effects.overall <- sum( (n.subcl/n)*effects.subcl)
se.overall <- sqrt( sum( (n.subcl/n)^2*vars.subcl ) )

print(effects.overall)
print(se.overall)


######## steps 2-5: full matching ###########
full.out <- matchit(treat ~ age + educ + black + hisp + married + 
                      + nodegr + re74 + re75, data = lalonde, 
                    method = "full", estimand = "ATE") 

print(full.out)
print(summary(m.out, standardize = TRUE))
bal.tab(full.out, thresholds = c(m=0.1))
love.plot(full.out, binary = "std", thresholds = c(m = 0.1))

fit.full <- lm(re78 ~ treat + age + educ + hisp + 
              married + nodegr + re75 + u74 + u75,
            data = match.data(full.out))
summary(fit.full)
confint(fit.full)[2,]


######## steps 2-5: weighting ###########
library(WeightIt)
w.out <- weightit(treat ~ age + educ + black + hisp + married +
                    re74 + re75, data = lalonde, estimand = "ATE", 
                  method = "ps")
summary(w.out)

bal.tab(w.out, thresholds = c(m=0.1))
love.plot(w.out, binary = "std", thresholds = c(m = 0.1))

library(survey)
design.w <- svydesign(~1, weights = w.out$weights, data = lalonde)
fit.w <- svyglm(re78 ~ treat + age + educ + hisp + 
                  married + nodegr + re75 + u74 + u75, 
                design = design.w)
## the target estimand w.out was ATE
confint(fit.w)[2,]