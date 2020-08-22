
# 3.1 ---------------------------------------------------------------------

x <- c(6, 10, 8, 13)
y <- c(7, 9, 11, 11)

d <- x - y
dbar <- mean(d)
s_d <- sd(d)
n <- length(d)

# 1
t <- dbar/(s_d/sqrt(n))
t
# 2
n-1


# 3.3 ---------------------------------------------------------------------

d <-c(2, 5, 6, 8, -6, 4,
      18, -12, 17, -7, 16, 12)
dbar <- mean(d)
s_d <- sd(d)
n <- length(d)

# 1
alpha <- 0.05
t <- dbar/(s_d/sqrt(n))
t_alpha <- qt(1 - alpha, n-1)

print(c(t, t_alpha))
# H0을 기각할 수 있다 

# 2
# 신뢰구간
t_alpha_2 <- qt(1 - alpha/2, n-1)
l <- dbar - t_alpha_2*(s_d/sqrt(n))
u <- dbar + t_alpha_2*(s_d/sqrt(n))
print(c(l, u))


# 3.5 ---------------------------------------------------------------------

x <- c(12, 29, 16, 37, 28, 15)
y <- c(10, 28, 17, 35, 25, 16)
d <- x -y
dbar <- mean(d)
s_d <- sd(d)
n <- length(d)

alpha <- 0.05
t_alpha <- qt(1 - alpha, n-1)
t <- dbar / (s_d/sqrt(n))
print(c(t, t_alpha))
# H0을 기각할 수 없다


# 3.13 --------------------------------------------------------------------

left <- c(140, 90, 125, 130, 95, 121, 85, 97, 131, 110)
right <- c(138, 87, 110, 132, 96, 120, 86, 90, 129, 100)
d <- left - right 
dbar <- mean(d)
s_d <- sd(d)
n <- length(d)

# 1
alpha <- 0.05
t_alpha <- qt(1 - alpha, n-1)
t <- dbar / (s_d/sqrt(n))
print(c(t, t_alpha))
# H0을 기각할 수 있다

# 2
alpha <- 0.1
t_alpjha_2 <- qt(1 - alpha/2, n-1)
l <- dbar - t_alpha_2*(s_d/sqrt(n))
u <- dbar + t_alpha_2*(s_d/sqrt(n))
print(c(l, u))


# 4.1 ---------------------------------------------------------------------

n1 <- 100
p1hat <- 0.5
n2 <- 200
p2hat <- 0.7

# 1
alpha <- 0.05
z_alpha_2 <- qnorm(1 - alpha/2)
l <- (p1hat - p2hat) - z_alpha_2 * sqrt(p1hat*(1-p1hat)/n1 + p2hat*(1-p2hat)/n2)
u <- (p1hat - p2hat) + z_alpha_2 * sqrt(p1hat*(1-p1hat)/n1 + p2hat*(1-p2hat)/n2)
print(c(l, u))

# 2
alpha <- 0.05
z_alpha <-qnorm(1 - alpha)
phat <- (n1*p1hat + n2*p2hat)/(n1+n2)
z <- (p1hat - p2hat) / sqrt(phat * (1 - phat) *(1/n1 + 1/n2))
print(c(z, -z_alpha))
# H0을 기각한다


# 4.3 ---------------------------------------------------------------------

n1 <- 120
p1hat <- 52/120
n2 <- 150
p2hat <- 88/150

# 1
alpha <- 0.05 
z_alpha <- qnorm(1 - alpha)
phat <- (n1*p1hat + n2*p2hat) / (n1+n2)
z <-(p1hat - p2hat) / sqrt(phat * (1-phat) * (1/n1 + 1/n2))
print(c(z, -z_alpha))
# H0을 기각할 수 있다

# 1
alpha <- 0.05 
z_alpha <- qnorm(1 - alpha)
phat <- (n1*p1hat + n2*p2hat) / (n1+n2)
z <-(p1hat - p2hat) / sqrt(phat * (1-phat) * (1/n1 + 1/n2))
print(c(z, z_alpha))
# H0을 기각할 수 없다

# 2
alpha <- 0.05
z_alpha_2 <- qnorm(1 - alpha/2)
l <- (p1hat - p2hat) - z_alpha_2 * sqrt(p1hat*(1-phat)/n1 + p2hat*(1-p2hat)/n2)
u <- (p1hat - p2hat) + z_alpha_2 * sqrt(p1hat*(1-phat)/n1 + p2hat*(1-p2hat)/n2)
print(c(l, u))


# 4.7 ---------------------------------------------------------------------

n1 <- 85
x <- 21
n2 <- 120
y <- 11

p1hat <- x/n1
p2hat <- y/n2

alpha <- 0.01
z_alpha <- qnorm(1 - alpha)
phat <- (x+y) / (n1+n2)
z <- (p1hat - p2hat) / sqrt(phat * (1-phat) * (1/n1 + 1/n2))
print(c(z, z_alpha))
# H0을 기각할 수 있다 


# 4.15 --------------------------------------------------------------------

n1 <- 549
x <- 11
n2 <- 534
y <- 70

p1hat <- x/n1
p2hat <- y/n2

# 1
alpha <- 0.01
z_alpha <- qnorm(1 - alpha)
phat <- (x+y) / (n1+n2)
z <- (p1hat - p2hat) / sqrt(phat * (1-phat) * (1/n1 + 1/n2))
print(c(z, -z_alpha))
# H0을 기각할 수 있다 

# 2
alpha <- 0.05
z_alpha_2 <- qnorm(1 - alpha/2)
l <- (p1hat - p2hat) - z_alpha_2 * sqrt(p1hat*(1-p1hat)/n1 + p2hat*(1-p2hat)/n2)
u <- (p1hat - p2hat) + z_alpha_2 * sqrt(p1hat*(1-p1hat)/n1 + p2hat*(1-p2hat)/n2)
print(c(l,u))
