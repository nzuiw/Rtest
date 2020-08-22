
# 2.1 ------------------------------------------------------------------

# 1
n <- 70
xsum <- 852
x_xbar <- 215 # dev_sq_sum
s <- sqrt(x_xbar/(n-1))

xbar <- xsum / n
xbar
se <- s / sqrt(n)
se

print(c(xbar, se))

# 2
n <- 140
xsum <- 1653
x_xbar <- 464
s <- sqrt(x_xbar/(n-1))

xbar <- xsum / n
xbar
se <- s / sqrt(n)
se

print(c(xbar, se))

# 3
n <- 160
xsum <- 1985
x_xbar <- 475
s <- sqrt(x_xbar/(n-1))

xbar <- xsum / n
xbar
se <- s / sqrt(n)
se

print(c(xbar, se))


# 2.5 ---------------------------------------------------------------------

n <- 35
xbar <- 30.2
s <- 3.8
alpha <- 0.05
z_alpha_half <- qnorm(1 - alpha/2)
z_alpha_half
z_alpha_half*(s / sqrt(n))
l <- xbar - z_alpha_half*(s / sqrt(n))
u <- xbar + z_alpha_half*(s / sqrt(n))

print(c(l, u))


# 2.14 --------------------------------------------------------------------

# (1)

xbar <- (52.6 + 58.2) / 2
xbar
alpha <- 0.05
z_alpha_half <- qnorm(1 - alpha/2)
z_alpha_half

(58.2 - 52.6)/2  # z_alpha_half * (s/sqrt(n))
s_sq_n <- ((58.2 - 52.6)/2) / z_alpha_half # s/sqrt(n)
s_sq_n # s/sqrt(n)

# (2)

alpha <- 0.2
z_alpha_half <- qnorm(1 - alpha/2)
z_alpha_half

l <- xbar - z_alpha_half*s_sq_n
u <- xbar + z_alpha_half*s_sq_n

print(c(l, u))

# 2.17 --------------------------------------------------------------------

sigma <- 9
d <- 1.2
alpha <- 0.1
z_alpha_half <- qnorm(1 - alpha/2)

n <- z_alpha_half * (sigma / d)
n * n

# 153Έν


# 3.7 ---------------------------------------------------------------------

# (1)

n <- 70
mu_zero <- 20
sigma <- 5.6
c <- 21.31

z <- (c - mu_zero) / (sigma / sqrt(n))
z

alpha <- 1 - pnorm(z)
alpha

# (2)

alpha <- 0.05
z <- qnorm(1 - 0.05)
z

c <- z * (sigma / sqrt(n)) + mu_zero
c


# 3.13 --------------------------------------------------------------------

n <- 36
xbar <- 80.4
s <- 16.2
mu_zero <- 85

z <- (xbar - mu_zero)/(s / sqrt(n))
z

p <- pnorm(z)*2
p


# 3.17 --------------------------------------------------------------------

n <- 43
xbar <- 3246
s <- 757
mu_zero <- 3000

z <- (xbar - mu_zero) / (s / sqrt(n))
z
p <- pnorm(-z) * 2
p


# 3.20 --------------------------------------------------------------------

# (1)

z <- (64 - 65) / (5 / sqrt(64))
z
pnorm(z)

# (2)

# (3)

alpha <- 0.01
z_alpha <- qnorm(1 - alpha)
z_alpha

c <- z_alpha * (5/8) +65
c


# 4.1 ---------------------------------------------------------------------

# (1)

n <- 50
phat <- 31/n
se <- sqrt((phat * (1 - phat))/n)

print(c(phat, se))

# (2)

n <- 460
phat <- 52/n
se <- sqrt((phat * (1 - phat))/n)

print(c(phat, se))

# (1)

n <- 2500
phat <- 1977/n
se <- sqrt((phat * (1 - phat))/n)

print(c(phat, se))


# 4.10 --------------------------------------------------------------------

alpha <- 0.1
z_alpha_half <- qnorm(1 - alpha/2)
z_alpha_half
d <- 0.04

n <- ((z_alpha_half / d)^2) * 0.25
n


# 4.23 --------------------------------------------------------------------

# (1)

alpha <- 0.05
d <- 0.02
p <- 0.75
z_alpha_half <- qnorm(1 - alpha/2)
z_alpha_half

n <- ((z_alpha_half / d)^2) * p * (1 - p)
n

# p = 0.6
p <- 0.6
n <- ((z_alpha_half / d)^2) * p * (1 - p)
n

# p = 0.8
p <- 0.8
n <- ((z_alpha_half / d)^2) * p * (1 - p)
n

# (2)

alpha <- 0.05
z_alpha_half <- qnorm(1 - alpha/2)
z_alpha_half

p <- 0.75 #phat
n <- 100

l <- p - z_alpha_half*sqrt((p*(1-p))/n)
l
u <- p + z_alpha_half*sqrt((p*(1-p))/n)
u

print(c(l, u))

# (3)

pzero <- 0.6
phat <- 0.75
n <- 100

z <- (phat - pzero) / sqrt((pzero*(1-pzero))/n)
z

p <- pnorm(z)*2
p



# 5.1 ---------------------------------------------------------------------

# (2)

# 1
# 2
# 3


# 5.12 --------------------------------------------------------------------

alpha <- 0.1
z_alpha_half <- qnorm(1 - alpha/2)
z_alpha_half


# 5.22 --------------------------------------------------------------------

# (1)

alpha <- 0.05
z_alpha_half <- qnorm(1 - alpha/2)
z_alpha_half

xbar <- (22.028 + 24.772)/2
xbar
s <- ((24.772 - 22.028)*7)/(2*z_alpha_half)
s

# (2)
n <- 49
se <- s/sqrt(n)
se

# (3)
alpha <- 0.1
z_alpha_half <- qnorm(1 - alpha/2)
z_alpha_half
l <- xbar - z_alpha_half*se
u <- xbar + z_alpha_half*se
print(c(l, u))


# (4)

alpha <- 0.05
z_alpha_half <- qnorm(1 - alpha/2)
z_alpha_half

muzero <- 23.8
z <- (xbar - muzero) / (s/sqrt(n))
z


# 5.32 --------------------------------------------------------------------

phat <- 38/130
pzero <- 0.25
z <- (phat-pzero) / sqrt((pzero*(1-pzero))/130)
z

p <- pnorm(z)
p
