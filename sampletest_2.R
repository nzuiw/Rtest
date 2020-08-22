# sampletest --------------------------------------------------------------
# 1 -----------------------------------------------------------------------

# 2 -----------------------------------------------------------------------

x <- c(143, 92, 123, 132, 98, 120, 82, 98, 140)
y <- c(142, 89, 112, 130, 95, 116, 81, 95, 129)

# 1 검정
d <- x-y
d
dbar <- mean(d)
s <- sd(d)
n <- length(x)

alpha <- 0.05
t_alpha <- qt(1-alpha, n-1)
delta0 <- 0
t <- (dbar-delta0)/(s/sqrt(n))

print(c(t,t_alpha))

# H0을 기각할 수 있다

pvalue <- pt(t, n-1)
1 - pvalue

# 2
alpha <- 0.1
t_alpha_2 <- qt(1-alpha/2, n-1)
l <- dbar - t_alpha_2*(s/sqrt(n))
u <- dbar + t_alpha_2*(s/sqrt(n))
print(c(l,u))


# 3 -----------------------------------------------------------------------

n1 <- 60
x <- 9
n2 <- 170
y <- 49

p1hat <- x/n1
p2hat <- y/n2

# 약품을 뿌리는 것이 우박의 발생빈도를 줄일 수 있는가? 
# H0 : p1 = p2 대 H1 : p1 < p2
# H0 : p1-p2=0 대 H1 : p1-p2<0

phat <- (x+y)/(n1+n2)

z <- (p1hat-p2hat) / (sqrt(phat*(1-phat))*sqrt(1/n1+1/n2))
z
pvalue <- pnorm(z)
pvalue


# 4 -----------------------------------------------------------------------

x <- c(1, 2, 2, 3, 3, 4, 4, 5, 7, 8, 11, 11)
y <- c(5.32, 4.92, 5.1, 4.2, 3.9, 3.4, 3.1, 2.8, 2.54, 2.1, 1.3, 1.2)

xbar <- mean(x)
ybar <- mean(y)
n <- length(x)

s_xx <- sum((x-xbar)^2)
s_yy <- sum((y-ybar)^2)
s_xy <- sum((x-xbar)*(y-ybar))

beta1_hat <- s_xy / s_xx
beta0_hat <- ybar - beta1_hat*xbar

sse <- s_yy - s_xy^2/s_xx
s <- sqrt(sse/(n-2))

# 2
plot(x, y)
# x2 <- seq(0, 11, 0.1)
yhat <- beta0_hat + beta1_hat*x
lines(x, yhat)

# 3
alpha <- 0.05
t_alpha_2 <- qt(1-alpha/2, n-2)
l <- beta1_hat - t_alpha_2*(s/sqrt(s_xx))
u <- beta1_hat + t_alpha_2*(s/sqrt(s_xx))
print(c(l, u))

# 4
# H0 : beta1_hat = 0 대 H1 : beta1_hat < 0
beta10 <- 0
t <- (beta1_hat - beta10) / (s/sqrt(s_xx))
alpha <- 0.05
t_alpha <- qt(1-alpha, n-2)
print(c(t, -t_alpha))
# H0을 기각할 수 있다 

# 5
# yhat <- beta0_hat + beta1_hat*x
xstar <- 9
beta0_hat + beta1_hat*xstar
alpha <- 0.05
t_alpha_2 <- qt(1-alpha/2, n-2)
l <- (beta0_hat + beta1_hat*xstar) - t_alpha_2 * s * sqrt(1 + 1/n + (xstar-xbar)^2/s_xx)
u <- (beta0_hat + beta1_hat*xstar) + t_alpha_2 * s * sqrt(1 + 1/n + (xstar-xbar)^2/s_xx)
print(c(l, u))

# 6
sst <- s_yy
ssr <- s_xy^2/s_xx
R_2 <- ssr/sst
R_2

# 7
y
yhat
e <- y - yhat
e
plot(yhat, e)

# sampletest --------------------------------------------------------------
# 8 -----------------------------------------------------------------------

n1 <- 80
n2 <- 120
xbar <- 78.4
ybar <- 82.3
s1 <- 9.1
s2 <- 8.5

alpha <- 0.05
z_alpha_2 <- qnorm(1-alpha/2)
z <- (xbar - ybar) / sqrt(s1^2/n1 + s2^2/n2)
print(c(z, z_alpha_2))
# H0 기각할 수 있다 

# 9 -----------------------------------------------------------------------

n1 <- 6
n2 <- 6
xbar <- 43.2
ybar <- 54.7
s1 <- 6.2
s2 <- 7.4

s1/s2 #표본표준편차가 같다고 가정 

sp_2 <- ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2)
s <- sqrt(sp_2)
t <- (xbar - ybar) / s*sqrt(1/n1+1/n2)
t
pt(t, n1+n2-2)
