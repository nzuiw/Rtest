
# 2.1 (3) -----------------------------------------------------------------
qt(0.01, 11)

# 2.2 (1) -----------------------------------------------------------------
qt(0.025, 27)
qt(1 - 0.025, 27)


# 2.3 (4) -----------------------------------------------------------------
qt(0.25, 19)
qt(0.75, 19)

# 2.4 (4) -----------------------------------------------------------------
pt(2.764, 10) - pt(-1.372, 10)

# 2.5 (4) -----------------------------------------------------------------
qt(0.01, 11)

# 2.6 ---------------------------------------------------------------------
qt(1 - 0.05, 5)
qt(1 - 0.05, 10)
qt(1 - 0.05, 15)
qt(1 - 0.05, 20)
qt(1 - 0.05, 29)

# 2.8 (5) -----------------------------------------------------------------
qt(0.02, 21) # -c
qt(0.98, 21) # c

# 3.2 ---------------------------------------------------------------------
data <- c(12, 16, 15, 20, 17, 11, 18)
n <- length(data)
xbar <- mean(data)
s <- sd(data)
alpha <- 0.05

t_alpha_half <- qt(1 - alpha/2, n-1)

l <- xbar - t_alpha_half *(s/sqrt(n))
u <- xbar + t_alpha_half *(s/sqrt(n))
print(c(l, u))

# 3.7 ---------------------------------------------------------------------
n <- 12
alpha <- 0.05
l <- 18.6
u <-26.2

t_alpha_half <- qt(1 - alpha/2, n-1)
t_alpha_half

# 1
xbar <- (l + u)/2
s <- ((u - l) * sqrt(n)) / (2*t_alpha_half)

# 2
alpha <- 0.02
t_alpha_half <- qt(1 - alpha/2, n-1)
l <- xbar - t_alpha_half*(s/sqrt(n))
u <- xbar + t_alpha_half*(s/sqrt(n))
print(c(l, u))

# 3.17 --------------------------------------------------------------------
data <- c(6.4, 4.3, 5.7, 4.9, 6.5, 5.9, 6.4, 5.1)
n <- length(data)
xbar <- mean(data)
s <- sd(data)

# 1
alpha <- 0.05
mu0 <- 5
t <-(xbar - mu0) / (s/sqrt(n))
t_alpha <- qt(1 - 0.05, n-1)
print(c(t, t_alpha))

# t > t_alpha? 귀무가설 기각 o

# 2
t_alpha_half <- qt(1 - alpha/2, n-1)
l <- xbar - t_alpha_half*(s/sqrt(n))
u <- xbar + t_alpha_half*(s/sqrt(n))
print(c(l, u))

# 4.3 ---------------------------------------------------------------------


# 4.5 ---------------------------------------------------------------------
data <- c(1.8, 10.6, -1.2, 12.9, 15.1, -2.0, 6.25, 10.8)
n <- length(data)
xbar <- mean(data)
s <- sd(data)

# 1
alpha <- 0.1
t_alpha_half <- qt(1 - alpha/2, n-1)
l <- xbar - t_alpha_half*(s/sqrt(n))
u <- xbar + t_alpha_half*(s/sqrt(n))
print(c(l, u))

# 2
mu0 <- 10
alpha <- 0.05
t_alpha_half <- qt(1 - alpha/2, n-1)
l <- xbar - t_alpha_half*(s/sqrt(n))
u <- xbar + t_alpha_half*(s/sqrt(n))
print(c(l, u))
# 기각 못함

# 3
t <- (xbar - mu0) / (s/sqrt(n))
alpha <- 0.05
t_alpha_half <- qt(1 - alpha/2, n-1)
print(c(t, t_alpha_half))
# |t| < t_alpha_half 기각할수없다  


# 5.4 ---------------------------------------------------------------------

# 1
data <- c(12, 18, 9, 15, 4)
s <- sd(data)
n <- length(data)

# 2
alpha <- 0.05
l <- s * sqrt((n-1) / qchisq(1 - alpha/2, n-1))
u <- s * sqrt((n-1) / qchisq(alpha/2, n-1))

print(c(l, u))

# 3


# 5.9 ---------------------------------------------------------------------

# 1
n <- 10
alpha <- 0.05
s <- 0.81 * sqrt(qchisq(1 - alpha/2, n-1) / (n-1))
s  

# 2
alpha <- 0.1
l <- s * sqrt((n-1)/qchisq(1 - alpha/2, n-1))
u <- s * sqrt((n-1)/qchisq(alpha/2, n-1))

print(c(l, u))
  
# 5.11 --------------------------------------------------------------------

n <- 19
alpha <- 0.05

s <- sqrt(18/qchisq(1 - alpha/2, n-1))

# 1
alpha <- 0.1
l <- s * sqrt((n-1)/qchisq(1 - alpha/2, n-1))
u <- s * sqrt((n-1)/qchisq(alpha/2, n-1))

print(c(l, u))

# 2
chi <- ((n-1)*s^2)/81
alpha <- 0.05
qchisq(1 - alpha/2, n-1)
qchisq(alpha/2, n-1)
