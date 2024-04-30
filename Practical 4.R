# Appendix code

# Attaching the 'who' dataset
attach(who)

# Plotting various variables against 'life'
plot(expenditure, life)
plot(tobacco, life)
plot(alcohol, life)
plot(obesity, life)

# Computing correlations between 'life' and other variables
cor(expenditure, life)
cor(log(expenditure), life)
cor(tobacco, life)
cor(alcohol, life)
cor(obesity, life)


# Load the MASS package for the stdres function
library(MASS)

# Define the linear models
lmf <- lm(life ~ log(expenditure) + tobacco + alcohol + obesity)
lm1 <- lm(life ~ tobacco + alcohol + obesity)
lm2 <- lm(life ~ log(expenditure) + alcohol + obesity)
lm3 <- lm(life ~ log(expenditure) + obesity)
lm4 <- lm(life ~ log(expenditure) + alcohol)

# Use the stdres function from the MASS package to get standardized residuals from lmf
evals <- stdres(lmf)

# The rest of the code can follow as previously mentioned
par(mfrow = c(1, 2))
hist(evals)
qqnorm(evals)
abline(0, 1)

ks.test(evals, pnorm, 0, 1)

# You can then proceed with ANOVA comparisons as needed
anova(lmf, lm1)
anova(lm2, lm3)
anova(lm2, lm4)

summary(lmf)
# And get the summary for any of the models
summary(lm2)
