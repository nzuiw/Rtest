
# 10장 연습문제 ----------------------------------------------------------------


# 1 -----------------------------------------------------------------------

n <- 140
x_sum <-1653
dev_sq_sum <- 464

x_bar <- x_sum / n
x_bar

s <- sqrt(dev_sq_sum / (n-1))
se <- s / sqrt(n)
se


# 2 -----------------------------------------------------------------------

n <- 48
x_bar <- 86.5
s <- 7.9
alpha <- 0.1
z_alpha_2 <- qnorm(1 - alpha/2)
z_alpha_2   #qnorm(0.95)와 같다

d <- z_alpha_2 * s / sqrt(n)

lower <- x_bar - d
upper <- x_bar + d

print(c(lower, upper))


# 그래프 그리기 -----------------------------------------------------------------

?plot
?seq
x <- seq(-1.0, 2.0, by = 0.1)
x
y <- x - 2*x^2
plot(x, y, type = 'l', col = "red")
?line


# 3 -----------------------------------------------------------------------

# Y~B(n. p)
y <- seq(0, 200, by = 1)
prob <- dbinom(y, 200, 0.9) #y의 확률 (그래프의 높이가 될 것)

?dbinom
plot(y, prob, type = 'l')

barplot(prob, names.arg = y) #높이만(prob) 
?barplot

# 4 -----------------------------------------------------------------------

n <- 38
xbar <- 68
s <- 12
#(alpha <- 0.05 해놓으면 신회구간 변할 때마다 바꾸기 쉬움)
z_alpha <- abs(qnorm(0.025))
# qnorm(1 - 0.025)

lower <- xbar - z_alpha*(12 / sqrt(n))
upper <- xbar + z_alpha*(12 / sqrt(n))
print(c(lower, upper))


# 5 -----------------------------------------------------------------------

sigma <- 4.5
d <- 1.89
alpha <- 0.01

z_alpha_half <- qnorm(1 - alpha/2)
z_alpha_half
n <- (z_alpha_half*sigma/d)^2
n

print(c(n, ceiling(n)))
print(c(n, floor(n)))
print(c(n, round(n))) #반올림 
round(1.634) #2
round(1.634, 2) # 1.63


# 10 -----------------------------------------------------------------------

# (1)

c <- 21.31
mu0 <- 20
sigma <- 5.6
n <- 70

a <- (c - mu0) / (sigma/sqrt(n))
a

prob <- 1 - pnorm(a)
prob

# (2)

alpha <- 0.05
mu0 <- 20
sigma <- 5.6
n <- 70

qnorm(1-alpha)
abs(qnorm(alpha))

c <- mu0 + abs(qnorm(alpha))*(sigma/sqrt(n))
c


# 11 ----------------------------------------------------------------------

mu0 <- 1100
n <- 80
xbar <- 1060
sd <- 210

# (1)
alpha <- 0.01
z_alpha_half <- qnorm(1 - alpha/2)
z_alpha_half
z <- (xbar - mu0)/(sd/sqrt(n))
z
#H0 기각x

# (2)

alpha <- 0.1
z_alpha_half <- qnorm(1 - alpha/2)
z_alpha_half
z <- (xbar - mu0)/(sd/sqrt(n))
z
#H0를 기각 o

# p-값 구하기 

pvalue <- 2*(1 - pnorm(abs(z)))
pvalue

print(c(z, z_alpha_half, pvalue))
# 최소 유의수준 (p) = 8.84%


# 12 ----------------------------------------------------------------------

mu0 <- 15
n <- 70
xbar <- 14.6
sd <- 3.0

# (1)
alpha <- 0.025
z_alpha <- qnorm(1 - alpha)
z_alpha

z <- (xbar - mu0) / (sd / sqrt(n))
z


# 12 ----------------------------------------------------------------------

# mu = 새로운 치료법을 시행하여 회복되는 데 걸리는 시간의 평균 (일)
# 
mu_zero <- 15
xbar <- 14.6
s <- 3.0
n <- 70

alpha <- 0.025
z_alpha <- qnorm(1 - alpha)
z_alpha <- -qnorm(alpha)
z_alpha # 

z <- (xbar - mu_zero) / (s / sqrt(n))
z

# p-값 
p <- pnorm(z)
p

print(c(z, z_alpha, p))



# 13 ----------------------------------------------------------------------

# (1)

phat <- 0.598
alpha <- 0.05
n <- 750

z_alpha_half <- qnorm(1 - alpha/2)
z_alpha_half

phat - z_alpha_half * sqrt(phat * ((1-phat)/n))
phat + z_alpha_half * sqrt(phat * ((1-phat)/n))

# (2)

phat <- 0.042
alpha <- 0.1
n <- 750

z_alpha_half <- qnorm(1 - alpha/2)
z_alpha_half

phat - z_alpha_half * sqrt(phat * ((1-phat)/n))
phat + z_alpha_half * sqrt(phat * ((1-phat)/n))


# 14 ----------------------------------------------------------------------

n <- 500
phat <- 228/500
alpha <- 0.05
pzero <- 0.5

# 검정 (0.05)

z <- (phat - pzero) / sqrt(pzero*(1 - pzero)/n)
z

z_alpha_half <- qnorm(1 - alpha)
z_alpha_half
-z_alpha_half

# p-값 구하기

p <- pnorm(z)
p 

print(c(z, -z_alpha_half, p))

# (-1.96773982 -1.64485363  0.02454899)

