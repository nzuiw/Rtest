# 시뮬레이션 -------------------------------------------------------------------

# 1-1
# N(3, 4^2) 인 히스토그램 
# n = 30일 때
# xbar의 분포는?

mu  <- 3
sigma <- 4
trials <- 10000
n <- 30

data <- rnorm(n*trials, mu, sigma)
data <- matrix(data, nrow = trials, ncol = n)
xbar <- apply(data, 1, mean) # 1 = 행에 따라서
hist(xbar, prob = T)

x <- seq(-2, 8, by = 0.2)
hist(xbar, breaks = x, prob = T)
lines(x, dnorm(x, mu, sigma/sqrt(n)), col = 'blue')

# 1-2
# z = (xbar - mu)/(sigma/sqrt(n)) 의 분포는?

z <- (xbar - mu)/(sigma/sqrt(n))
hist(z, prob = T)
x <- seq(-4, 4, by = 0.1)
hist(z, breaks = x, prob = T)
lines(x, dnorm(x, 0, 1), col='blue')

# 1-3
# z = (xbar - mu)/(s/sqrt(n)) 의 분포는?
#(xbar <- apply(data, 1, mean))

s <- apply(data, 1, sd)
t <- (xbar - mu)/(s/sqrt(n))
x <- seq(-6, 6, by = 0.2)
hist(t, breaks = x, prob = T)
lines(x, dnorm(x, 0, 1), col = 'blue')
lines(x, dt(x, n-1), col = 'red')

# 2-1
# N(3, 4^2) 인 히스토그램 
# n = 7일 때
# xbar의 분포는?

mu  <- 3
sigma <- 4
trials <- 10000
n <- 7

data <- rnorm(n*trials, mu, sigma)
data <- matrix(data, nrow = trials, ncol = n)
xbar <- apply(data, 1, mean) # 1 = 행에 따라서
hist(xbar, prob = T)

x <- seq(-3, 10, by = 0.5)
hist(xbar, breaks = x, prob = T)
lines(x, dnorm(x, mu, sigma/sqrt(n)), col = 'blue')

# 2-2
# z = (xbar - mu)/(sigma/sqrt(n)) 의 분포는?

z <- (xbar - mu)/(sigma/sqrt(n))
hist(z, prob = T)
x <- seq(-5, 5, by = 0.5)
hist(z, breaks = x, prob = T)
lines(x, dnorm(x, 0, 1), col='blue')


# 2-3
# z = (xbar - mu)/(s/sqrt(n)) 의 분포는?
#(xbar <- apply(data, 1, mean))

s <- apply(data, 1, sd)
t <- (xbar - mu)/(s/sqrt(n))
hist(t, prob = T)
x <- seq(-12, 12, by = 0.5)
hist(t, breaks = x, prob = T)
lines(x, dnorm(x, 0, 1), col = 'blue')
lines(x, dt(x, n-1), col = 'red')