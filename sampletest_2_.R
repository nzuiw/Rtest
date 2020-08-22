
# 5 -----------------------------------------------------------------------

x <- c(8, 5, 7, 6, 9, 7)
y <- c(2, 6, 4, 7, 6)

#s_p =?

n1 <- 6
n2 <- 5
xbar <- mean(x)
ybar <- mean(y)
s1 <- sd(x)
s2 <- sd(y)

s_p_2 <- ((n1-1)*s1^2 + (n2-1)*s2^2) / (n1+n2-2)
s_p_2


# 6 -----------------------------------------------------------------------

# mu1 - mu2 ?
x <- c(44, 44, 56, 46, 47, 38, 58, 53, 49, 35, 46, 30, 41)
y <- c(35, 47, 55, 29, 40, 39, 32, 41, 42, 57, 51, 39)
alpha <- 0.05

n1 <- length(x)
n2 <- length(y)
xbar <- mean(x)
ybar <- mean(y)
s1 <- sd(x)
s2 <- sd(y)
s1/s2
s_p_2 <-((n1-1)*s1^2 + (n2-1)*s2^2) / (n1+n2-2)
s <- sqrt(s_p_2)

t_alpha_2 <- qt(1-alpha/2,n1+n2-2)

l <- (xbar - ybar) - t_alpha_2*s*sqrt(1/n1+1/n2)
u <- (xbar - ybar) + t_alpha_2*s*sqrt(1/n1+1/n2)

print(c(l,u))


# 13장 전체문제  ---------------------------------------------------------------

x <- c(3, 3, 4, 5, 6, 6, 7, 8, 8, 9)
y <- c(9, 5, 12, 9, 14, 16, 22, 18, 24, 22)

# 모수추정
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

alpha <- 0.05
t_alpha_2 <- qt(1-alpha/2, n-2)

l <- beta1_hat - t_alpha_2 * (s/sqrt(s_xx))
u <- beta1_hat + t_alpha_2 * (s/sqrt(s_xx))
print(c(l,u))

l <- beta0_hat - t_alpha_2 * s * sqrt(1/n + xbar^2/s_xx)
u <- beta0_hat + t_alpha_2 * s * sqrt(1/n + xbar^2/s_xx)
print(c(l,u))

# y = beta0_hat + beta1_hat * x
x1 <- 6
l <- beta0_hat + beta1_hat * x1 - t_alpha_2 * s * sqrt(1/n + (x1-xbar)^2/s_xx)
u <- beta0_hat + beta1_hat * x1 + t_alpha_2 * s * sqrt(1/n + (x1-xbar)^2/s_xx)
print(c(l,u))

x2 <- 9.5
l <- beta0_hat + beta1_hat * x2 - t_alpha_2 * s * sqrt(1/n + (x2-xbar)^2/s_xx)
u <- beta0_hat + beta1_hat * x2 + t_alpha_2 * s * sqrt(1/n + (x2-xbar)^2/s_xx)
print(c(l,u))

xstar <- 6
l <- (beta0_hat + beta1_hat*xstar) - t_alpha_2 * s* sqrt(1 + 1/n + (xstar-xbar)^2/s_xx)
u <- (beta0_hat + beta1_hat*xstar) + t_alpha_2 * s* sqrt(1 + 1/n + (xstar-xbar)^2/s_xx)
print(c(l,u))

sst <- s_yy
ssr <- s_xy^2/s_xx
R <- ssr/sst

