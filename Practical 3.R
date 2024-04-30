# Attaching the dataset
attach(dyslexia)

# Setting up the linear model with the predictor variable
lm.linear <- lm(rad~bpvt, dyslexia)

# Setting up the null model with only the intercept
lm.null <- lm(rad~1, dyslexia)

# Running ANOVA to compare the null model with the linear model
anova(lm.null, lm.linear)

# The result give a p-value of 0.000985,so we reject null hypothesis of  no significant relationship between RAD and BPVT

lm.quad <- lm(rad ~ bpvt + I(bpvt^2), dyslexia)

# Define the quadratic model
lm.quad <- lm(rad ~ bpvt + I(bpvt^2), dyslexia)

# Null hypothesis is that the coefficient of the quadratic term is zero
# Compare the linear model with the quadratic model
anova(lm.linear, lm.quad)

# The result give a p-value of 0.6135,we accept null hypothesis.......

xvalues <- data.frame(bpvt=seq(from=10, to=80, length=100))
predictions.PI <- data.frame(predict.lm(lm.linear, xvalues, interval="prediction", level=0.95))
plot(bpvt, rad, ylim=c(-50, 70), xlab="BPVT", ylab="RAD")
abline(lm.linear)
lines(xvalues$bpvt, predictions.PI$lwr, lty=4)
lines(xvalues$bpvt, predictions.PI$upr, lty=4)
legend("bottomright", c("fitted line", "95% prediction interval"), lty=c(1, 4))

