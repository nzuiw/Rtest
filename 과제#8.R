# 4.1  --------------------------------------------------------------------

x <- c(0, 1, 6, 3, 5)
y <- c(4, 3, 0, 2, 1)

# 1

n <- length(x)
xbar <- mean(x)
ybar <- mean(y)
s_xx <- sum((x-xbar)^2)
s_yy <- sum((y-ybar)^2)
s_xy <- sum((x-xbar)*(y-ybar))

beta1_hat <- s_xy/s_xx
beta0_hat <- ybar - beta1_hat*xbar

sse <- s_yy - s_xy^2/s_xx
mse <- sse/(n-2)
s <- sqrt(mse)

print(c(beta0_hat, beta1_hat, mse))

# 2

alpha <- 0.05
beta10 <- 0 
t0 <- (beta1_hat - beta10) / sqrt(mse/s_xx)
t_alpha_2 <- qt(1 - alpha/2, n-2)
print(c(t0, t_alpha_2))
# H0을 기각할 수 있다

# 3

x_star <- 2.5
y_hat <- beta0_hat + beta1_hat * x_star

alpha <- 0.1
t_alpha_2 <- qt(1 - alpha/2, n-2)
l <- y_hat - t_alpha_2 *s*sqrt(1/n + (x_star - xbar)/s_xx)
u <- y_hat + t_alpha_2 *s*sqrt(1/n + (x_star - xbar)/s_xx)
print(c(y_hat, l, u))

# 4 

beta0_hat

alpha <- 0.1
t_alpha_2 <- qt(1 - alpha/2, n-2)
l <- beta0_hat - t_alpha_2*s*sqrt(1/n + xbar^2/s_xx)
u <- beta0_hat + t_alpha_2*s*sqrt(1/n + xbar^2/s_xx)
print(c(l, u))


# 4.3 ---------------------------------------------------------------------

x <- c(1, 2, 3, 4, 5)
y <- c(0.9, 2.1, 2.4, 3.3, 3.8)

# 1

xbar <- mean(x)
ybar <- mean(y)
n <- length(x)
s_xx <- sum((x-xbar)^2)
s_yy <- sum((y-ybar)^2)
s_xy <- sum((x-xbar)*(y-ybar))

beta1_hat <- s_xy/s_xx
beta0_hat <- ybar - beta1_hat*xbar

sse <- s_yy - s_xy^2/s_xx
mse <- sse/(n-2)
s <- sqrt(mse)

print(c(beta0_hat, beta1_hat, mse))

# 2

alpha <- 0.05
beta10 <- 1
t_alpha_2 <- qt(1-alpha/2, n-2)
t0 <- (beta1_hat-beta10)/(s/sqrt(s_xx))

print(c(t0, t_alpha_2))
# H0을 기각할 수 있다

# 3

xstar <- 3.5
yhat <- beta0_hat + beta1_hat * xstar

alpha <- 0.05
t_alpha_2 <-qt(1 - alpha/2, n-2)
l <- yhat - t_alpha_2*s*sqrt(1/n+ (xstar-xbar)^2/s_xx)
u <- yhat + t_alpha_2*s*sqrt(1/n+ (xstar-xbar)^2/s_xx)

print(c(yhat, l, u))

# 4

beta0_hat

alpha <- 0.1
t_alpha_2 <- qt(1-alpha/2, n-2)
l <- beta0_hat - t_alpha_2*s*sqrt(1/n + xbar^2/s_xx)
u <- beta0_hat + t_alpha_2*s*sqrt(1/n + xbar^2/s_xx)
print(c(l, u))


# 4.7 ---------------------------------------------------------------------

n <- 15
xar <- 10.8
ybar <- 122.7
s_xx <- 70.6
s_yy <- 98.5
s_xy <- 68.3

beta1_hat <- s_xx/s_xy
beta0_hat <- ybar - beta1_hat * xbar

sse <- s_yy - s_xy^2/s_xx
mse <- sse/(n-2)
s <- sqrt(mse)

# 1

xstar <- 12
alpha <- 0.05
t_alpha_2 <- qt(1-alpha/2, n-2)

yhat <- beta0_hat + beta1_hat*xstar

l <- yhat - t_alpha_2*s*sqrt(1/n + (xstar-xbar)/s_xx)
u <- yhat + t_alpha_2*s*sqrt(1/n + (xstar-xbar)/s_xx)

print(c(l, u))
u-l
# 2

xstar <- 15
alpha <- 0.05
t_alpha_2 <- qt(1-alpha/2, n-2)

yhat <- beta0_hat + beta1_hat*xstar

l <- yhat - t_alpha_2*s*sqrt(1/n + (xstar-xbar)/s_xx)
u <- yhat + t_alpha_2*s*sqrt(1/n + (xstar-xbar)/s_xx)

print(c(l, u))
u-l
# 3
# [1] 130.4997 133.5064
# [1] 133.4444 136.7638


# 4.9 ---------------------------------------------------------------------

x <- c(0, 1, 2, 3, 4)
y <- c(195, 216, 244, 260, 284)

n <- length(x)
xbar <- mean(x)
ybar <- mean(y)
s_xx <- sum((x-xbar)^2)
s_yy <- sum((y-ybar)^2)
s_xy <- sum((x-xbar)*(y-ybar))

beta1_hat <- s_xy/s_xx
beta0_hat <- ybar - beta1_hat*xbar

# 1

yhat <- beta0_hat + beta1_hat*x
plot(x, yhat)

# 2

alpha <- 0.05
beta10 <- 0
t_alpha_2 <- qt(1-alpha/2, n-2)
t0 <- (beta1_hat-beta10)/(s/sqrt(s_xx))

print(c(t0, t_alpha_2))
# H0을 기각할 수 있다

# 3

xstar <- 9
yhat <- beta0_hat + beta1_hat*xstar
yhat


# 5.1 ---------------------------------------------------------------------

n <- 14
xbar <- 1.2
ybar <- 5.1
s_xx <- 14.10
s_xy <- 2.31
s_yy <- 2.01

ssr <- s_xy^2/s_xx
sse <- s_yy-s_xy^2/s_xx
sst <- s_yy

ssr/sst


# 5.3 ---------------------------------------------------------------------

s_xx <- 92
s_yy <- 457
s_xy <- 160

sst <- s_yy
ssr <- s_xy^2/s_xx
sse <- s_yy - s_xy^2/s_xx

ssr/sst


# 5.5 ---------------------------------------------------------------------

x <- c(200.8, 194.6, 183.5, 190.5, 210.2, 170.5, 220.0)
y <- c(207.0, 199.0, 188.0, 191.2, 211.0, 176.2, 218.0)

n <- length(x)
xbar <- mean(x)
ybar <- mean(y)
s_xx <- sum((x-xbar)^2)
s_yy <- sum((y-ybar)^2)
s_xy <- sum((x-xbar)*(y-ybar))

sst <- s_yy
ssr <- s_xy^2/s_xx
sse <- s_yy - s_xy^2/s_xx

# 1
ssr/sst

# 2
sqrt(ssr/sst)

# 3


# 5.7 ---------------------------------------------------------------------


# 6.1 ---------------------------------------------------------------------

yhat <- c(11.3, 14.8, 18.4, 22.0, 25.5, 27.0, 31.2, 32.7, 34.1, 
          36.2, 39.8, 43.4, 45.5, 46.2, 46.9, 46.9)
e <- c(-0.3, 0.2, -5.4, 2.0, 0.5, 5.0, -9.2, 6.3, 12.9, 
       -4.2, -14.8, 7.6, -1.5, 15.8, 1.1, -16.9)
plot(yhat, e)


# 6.3 --------------------------------------------------------------------

yhat <- c(2.2, 3.1, 2.5, 3.3, 2.3, 3.6, 2.6, 
          2.5, 3.0, 3.2, 2.9, 3.3, 2.7, 3.2)
e <- c(-1, -2, 3, -3, -1, 5, 0, 
       0, 3, -2, 2, -5, 0, 1)
seq <- c(9, 6, 13, 1, 7, 14, 8, 
         3, 12, 4, 11, 2, 10, 5)

# 1
plot(yhat, e)
plot(seq, e)

# 2

