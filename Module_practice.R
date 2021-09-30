###
#
# Module Practice
#
###

#######################################
#
#
# Challenge 9
# Introduction to statistical inference
#
#
#
#######################################

n <- 1000
mu <- 3.5
sigma <- 4
v <- rnorm(n, mu, sigma)
s <- sample(v, size = 30, replace = FALSE)
m <- mean(s)
m

sd <- sd(s)
sd

lower <- m - qnorm(1 - 0.05/2) * sem  # (1-alpha)/2 each in the upper and lower tails of the distribution
upper <- m + qnorm(1 - 0.05/2) * sem  # (1-alpha)/2 each in the upper and lower tails of the distribution
ci <- c(lower, upper)
ci

lambda <- 14
n <- 10
pop_se <- sqrt(lambda/n)  # the estimated SE
pop_se

x <- NULL
for (i in 1:1000) {
  x[i] <- mean(rpois(n = n, lambda = lambda))
}
hist(x, breaks = seq(from = lambda - 4 * sqrt(lambda)/sqrt(n), to = lambda + 
                       4 * sqrt(lambda)/sqrt(n), length.out = 20), probability = TRUE)

sd <- sd(x)  # st dev of the sampling distribution
sd

qqnorm(x)
qqline(x)

n <- 100
pop_se <- sqrt(lambda/n)  # the estimated SE
pop_se

x <- NULL
for (i in 1:1000) {
  x[i] <- mean(rpois(n = n, lambda = lambda))
}
hist(x, breaks = seq(from = lambda - 4 * sqrt(lambda)/sqrt(n), to = lambda + 
                       4 * sqrt(lambda)/sqrt(n), length.out = 20), probability = TRUE)

sd <- sd(x)  # st dev of the sampling distribution
sd

qqnorm(x)
qqline(x)

curve(dnorm(x, 0, 1), -4, 4, ylim = c(0, 0.8))
z <- (x - lambda)/pop_se
hist(z, breaks = seq(from = -4, to = 4, length.out = 20), probability = TRUE, 
     add = TRUE)

n <- 100
x <- NULL
for (i in 1:1000) {
  x[i] <- sum(rpois(n = n, lambda = lambda))
}
hist(x, breaks = seq(min(x), max(x), length.out = 20), probability = TRUE)

#
#
#
############ Challenge 1 ###############3
#
#
#

n <- 1000
x <- 856
phat <- x/n  # our estimate of pi
phat

n * phat

n * (1 - phat)

pop_se <- sqrt((phat) * (1 - phat)/n)

curve(dnorm(x, mean = phat, sd = pop_se), phat - 4 * pop_se, phat + 4 * pop_se)
upper <- phat + qnorm(0.975) * pop_se
lower <- phat - qnorm(0.975) * pop_se
ci <- c(lower, upper)
polygon(cbind(c(ci[1], seq(from = ci[1], to = ci[2], length.out = 1000), ci[2]), 
              c(0, dnorm(seq(from = ci[1], to = ci[2], length.out = 1000), mean = phat, 
                         sd = pop_se), 0)), border = "black", col = "gray")
abline(v = ci)
abline(h = 0)

mu <- 0
sigma <- 1
curve(dnorm(x, mu, 1), mu - 4 * sigma, mu + 4 * sigma, main = "Normal Curve=red\nStudent's t=blue", 
      xlab = "x", ylab = "f(x)", col = "red", lwd = 3)
for (i in c(1, 2, 3, 4, 5, 10, 20, 100)) {
  curve(dt(x, df = i), mu - 4 * sigma, mu + 4 * sigma, main = "T Curve", xlab = "x", 
        ylab = "f(x)", add = TRUE, col = "blue", lty = 5)
}

n <- 1e+05
mu <- 3.5
sigma <- 4
x <- rnorm(n, mu, sigma)
sample_size <- 30
s <- sample(x, size = sample_size, replace = FALSE)
m <- mean(s)
m

sd <- sd(s)
sd

sem <- sd(s)/sqrt(length(s))
sem

lower <- m - qnorm(1 - 0.05/2) * sem  # (1-alpha)/2 each in the upper and lower tails of the distribution
upper <- m + qnorm(1 - 0.05/2) * sem  # (1-alpha)/2 each in the upper and lower tails of the distribution
ci_norm <- c(lower, upper)
ci_norm

lower <- m - qt(1 - 0.05/2, df = sample_size - 1) * sem  # (1-alpha)/2 each in the upper and lower tails of the distribution
upper <- m + qt(1 - 0.05/2, df = sample_size - 1) * sem  # (1-alpha)/2 each in the upper and lower tails of the distribution
ci_t <- c(lower, upper)
ci_t

sample_size <- 5
s <- sample(x, size = sample_size, replace = FALSE)
m <- mean(s)
m

sd <- sd(s)
sd

sem <- sd(s)/sqrt(length(s))
sem

lower <- m - qnorm(1 - 0.05/2) * sem  # (1-alpha)/2 each in the upper and lower tails of the distribution
upper <- m + qnorm(1 - 0.05/2) * sem  # (1-alpha)/2 each in the upper and lower tails of the distribution
ci_norm <- c(lower, upper)
ci_norm

lower <- m - qt(1 - 0.05/2, df = sample_size - 1) * sem  # (1-alpha)/2 each in the upper and lower tails of the distribution
upper <- m + qt(1 - 0.05/2, df = sample_size - 1) * sem  # (1-alpha)/2 each in the upper and lower tails of the distribution
ci_t <- c(lower, upper)
ci_t



###############################
#
# 
# Module 10
# Classical hypothesis testing
#
#
###############################



library(curl)
f <- curl("https://raw.githubusercontent.com/fuzzyatelin/fuzzyatelin.github.io/master/AN588_Fall21/vervet-weights.csv")
d <- read.csv(f, header = TRUE, sep = ",", stringsAsFactors = FALSE)
head(d)

mean(d$weight)

mu <- 4.9
x <- d$weight
m <- mean(x)
s <- sd(x)
n <- length(x)
sem <- s/sqrt(n)

z <- (m - mu)/sem
z

p <- 1 - pnorm(z)
p

p <- pnorm(z, lower.tail = FALSE)
p

p <- 1 - pt(z, df = n - 1)
p

p <- pt(z, df = n - 1, lower.tail = FALSE)
p

t <- t.test(x = x, mu = mu, alternative = "greater")
t

lower <- m - qt(1 - 0.05/2, df = n - 1) * sem
upper <- m + qt(1 - 0.05/2, df = n - 1) * sem
ci <- c(lower, upper)
ci  # by hand

t <- t.test(x = x, mu = mu, alternative = "two.sided")
ci <- t$conf.int
ci  # using t test

#
#
#
######### Challenge 1 #########
#
#
#

mu <- 7.2
m <- 6.43
s <- 0.98
n <- 15
sem <- s/sqrt(n)

z <- (m - mu)/sem
z

p <- pt(z, df = n - 1, lower.tail = TRUE)
p

#
#
#
############## Challenge 2 #################
#
#
#
f <- curl("https://raw.githubusercontent.com/fuzzyatelin/fuzzyatelin.github.io/master/AN588_Fall21/colobus-weights.csv")
d <- read.csv(f, header = TRUE, sep = ",", stringsAsFactors = FALSE)
head(d)

x <- d$weight[d$sex == "male"]
y <- d$weight[d$sex == "female"]
par(mfrow = c(1, 2))
boxplot(x, ylim = c(4.5, 8), main = "Weight (kg)", xlab = "Males")
boxplot(y, ylim = c(4.5, 8), main = "Weight (kg)", xlab = "Females")

# Two-Sample T test of Un-equal variance 

m1 <- mean(x)
m2 <- mean(y)
mu <- 0  # you could leave this out... the default argument value is 0
s1 <- sd(x)
s2 <- sd(y)
n1 <- length(x)
n2 <- length(y)

t <- (m2 - m1 - mu)/sqrt(s2^2/n2 + s1^2/n1)
t

alpha <- 0.05
crit <- qt(1 - alpha/2, df = n - 1)  # identify critical values
crit

test <- t < -crit || t > crit  # boolean test
test <- abs(t) > crit
test

df <- (s2^2/n2 + s1^2/n1)^2/((s2^2/n2)^2/(n2 - 1) + (s1^2/n1)^2/(n1 - 1))
df

t <- t.test(x = x, y = y, mu = 0, alternative = "two.sided")
t

# Two-Sample T test of Un-equal variance 

s <- sqrt((((n1 - 1) * s1^2) + ((n2 - 1) * s2^2))/(n1 + n2 - 2))
t <- (m2 - m1 - mu)/(sqrt(s^2 * (1/n1 + 1/n2)))
t

df <- n1 + n2 - 2
df

t <- t.test(x = x, y = y, mu = 0, var.equal = TRUE, alternative = "two.sided")
t

var(x)/var(y)

vt <- var.test(x, y)
vt

#
#
#
########### Challenge 3 ############
#
#
#

# Paired t-test


f <- curl("https://raw.githubusercontent.com/fuzzyatelin/fuzzyatelin.github.io/master/AN588_Fall21/iqs.csv")
d <- read.csv(f, header = TRUE, sep = ",", stringsAsFactors = FALSE)
head(d)

x <- d$IQ.before
y <- d$IQ.after
par(mfrow = c(1, 2))
boxplot(x, ylim = c(100, 160), main = "IQ", xlab = "Before")
boxplot(y, ylim = c(100, 160), main = "IQ", xlab = "After")

x <- d$IQ.before - d$IQ.after
m <- mean(x)
mu <- 0  # can leave this out
s <- sd(x)
n <- length(x)
sem <- s/sqrt(n)
t <- (m - mu)/sem
t
alpha <- 0.05
crit <- qt(1 - alpha/2, df = n - 1)  # identify critical values
crit
test <- t < -crit || t > crit  # boolean test
test

t.test(x, df = n - 1, alternative = "two.sided")

# Testing Sample Proportions: One Sample Z Test

pop <- c(rep(0, 500), rep(1, 500))

pi <- 0.5
x <- NULL
n <- 10
for (i in 1:1000) {
  x[i] <- mean(sample(pop, size = n, replace = FALSE))  # taking the mean of a bunch of 0s and 1s yields the proportion of 1s!
}
m <- mean(x)
m

s <- sd(x)
s

pop_se <- sqrt(pi * (1 - pi)/n)
pop_se  # the se is an estimate of the sd of the sampling distribution

pop <- c(rep(0, 800), rep(1, 200))
pi <- 0.8
x <- NULL
n <- 10
for (i in 1:1000) {
  x[i] <- mean(sample(pop, size = n, replace = FALSE))  # taking the mean of a bunch of 0s and 1s yields the proportion of 1s!
}
m <- mean(x)
m
s <- sd(x)
s

pop_se <- sqrt(pi * (1 - pi)/n)
pop_se  # the se is an estimate of the sd of the sampling distribution

############ Challenge 4 ##############

# A neotropical ornithologist working in the western Amazon 
# deploys 30 mist nets in a 100 hectare (ha) grid. 
# She monitors the nets on one morning and records whether or not 
# she captures any birds in the net 
# (i.e., a “success” or “failure” for every net during a netting session). 
# The following vector summarizes her netting results:
v <- c(0, 1, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 
       1, 1, 0, 1, 0, 1, 1)

# What is H0?
#   What is HA?
#  Are both n×π and n×(1−π) > 5?
#   Calculate z and the p value associated with z
# Calculate the 95% CI around 
























