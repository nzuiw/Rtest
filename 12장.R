
# 예제 3 --------------------------------------------------------------------

n1 <- 50
xbar <- 453
s1 <- 80

n2 <- 100
ybar <- 401
s2 <- 60

alpha <- 0.05
z_alpha_half <- qnorm(1 - alpha/2)

l <- (xbar - ybar) - z_alpha_half * sqrt(s1^2/n1 + s2^2/n2)
u <- (xbar - ybar) + z_alpha_half * sqrt(s1^2/n1 + s2^2/n2)

print(c(l, u))


# 예제 4 --------------------------------------------------------------------

alpha <- 0.01
delta0 <- 0

z_alpha_half <- qnorm(1 - alpha/2)
z <- ((xbar - ybar) - delta0) / sqrt(s1^2/n1 + s2^2/n2)

print(c(z, z_alpha_half))
# 검정통계량 z 값이 z_a 보다 크므로 H0을 기각할 수 있다

pvalue <- 2 * (1 - pnorm(z))
pvalue


# 예제 5 --------------------------------------------------------------------

x <- c(8, 5, 7, 6, 9, 7)
y <- c(2, 6, 4, 7, 6)

s1 <- sd(x)
s2 <- sd(y)
n1 <- length(x)
n2 <- length(y)

sp_sq <- ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2)


# 예제 6 --------------------------------------------------------------------

# mu1 - mu2 ?
x <- c(44, 44, 56, 46, 47, 38, 58, 53, 49, 35, 46, 30, 41)
y <- c(35, 47, 55, 29, 40, 39, 32, 41, 42, 57, 51, 39)
alpha <- 0.05

s1 <- sd(x)
s2 <- sd(y)
s1/s2 # 두 모집단의 표준편차가 같다고 가정

n1 <- length(x)
n2 <- length(y)
xbar <- mean(x)
ybar <- mean(y)

sp <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2) / (n1+n2-2))

# 신뢰구간 구하기
t_alpha_half <- qt(1 - alpha/2, n1+n2-2)

l <- (xbar - ybar) - t_alpha_half*sp*sqrt(1/n1 + 1/n2)
u <- (xbar - ybar) + t_alpha_half*sp*sqrt(1/n1 + 1/n2)

print(c(l, u))


# 예제 7 --------------------------------------------------------------------

alpha <- 0.05
delta0 <- 0

# 검정통계량 
t_alpha <- qt(1 - alpha, n1+n2-2)
t <- ((xbar - ybar) - delta0) / (sp * sqrt(1/n1 + 1/n2))

print(c(t, t_alpha))
# t >= t_alpha 를 만족시키지 못하므로 H0을 기각할 수 없다
# 들판건조 목초 사용과 인공건조 목초 사용과 큰 차이가 없다

# pvalue <- pnorm(t)


# 예제 8 --------------------------------------------------------------------

# mu1 - mu2 ?
# t0* = ?
# t_alpha_half = ?

alpha <- 0.05
n1 <- 13
xbar <- 2.4
s1 <- 0.72
n2 <- 11
ybar <- 2.15
s2 <- 0.35
r <- n2 - 1

delta0 <- 0
s1/s2 # 적절하지 못함

t <- (xbar - ybar) / sqrt(s1^2/n1 + s2^2/n2)
t_alpha_half <- qt(1 - alpha/2, r)
print(c(t, t_alpha_half))

# H0을 기각할 수 없다





# 연습문제 --------------------------------------------------------------------




# 교재 12장 2절 
# 2.2 2.6 2.13 2.15 2.21


# 2.2 ---------------------------------------------------------------------

n1 <- 90
n2 <- 100
xbar <- 76.4
ybar <- 81.2
s1 <- 8.2
s2 <- 7.6

alpha <- 0.02
z_alpha_half <- qnorm(1 - alpha/2)
z_alpha_half

# mu1 - mu2 에 대한 신뢰구간
l <- (xbar - ybar) - z_alpha_half * sqrt(s1^2/n1 + s2^2/n2)
u <- (xbar - ybar) + z_alpha_half * sqrt(s1^2/n1 + s2^2/n2)

print(c(l, u))


# 연습시간 --------------------------------------------------------------------
# 2.6 ---------------------------------------------------------------------

n1 <- 66
n2 <- 38
xbar <- 305
ybar <- 311
s1 <- 29
s2 <- 40

# 1

# H0 : xbar - ybar = 0 vs H1: xbar - ybar != 0
alpha <- 0.01
z_alpha_half <- qnorm(1 - alpha/2)
z_alpha_half

delta0 <- 0

z <- ((xbar - ybar) - delta0)/sqrt(s1^2/n1 + s2^2/n2)
z

print(c(z, z_alpha_half))

# z절대값 > z_a 를 만족하지 못하므로, H0을 기각할 수 없다
# 차이가 있다고 볼 수 없다

# 2

alpha <- 0.01
z_alpha_half <- qnorm(1 - alpha/2)
z_alpha_half

l <- (xbar - ybar) - z_alpha_half * sqrt(s1^2/n1 + s2^2/n2)
u <- (xbar - ybar) + z_alpha_half * sqrt(s1^2/n1 + s2^2/n2)

print(c(l, u))


# 2.13 --------------------------------------------------------------------


# 2.15 --------------------------------------------------------------------
0.86/1.12
1.12/0.86

5.22/1.65
1.65/5.22

# 2.21 --------------------------------------------------------------------



# 예제 9 --------------------------------------------------------------------

d <- c(2, 8, 10, 6, 18, 10, 4, 
       26, 18, -8, 0, 32, 0, -4, 10)
dbar <- mean(d)
s <- sd(d)
n <- length(d)

# 1
alpha <- 0.05
t_alpha_2 <- qt(1 - alpha/2, n-1)

l <- dbar - t_alpha_2 * (s/sqrt(n))
u <- dbar + t_alpha_2 * (s/sqrt(n))

print(c(l, u))

# 2
alpha <- 0.01
t_alpha <- qt(1 - alpha, n-1)

delta0 <- 0
t <- (dbar - delta0) / (s/sqrt(n))

print(c(t, t_alpha))

pvalue <- 1 - pt(t, n-1)


# 예제 10 -------------------------------------------------------------------

n1 <- 100
x <- 88
n2 <- 150
y <- 126

alpha <- 0.05
z_alpha_2 <- qnorm(1 - alpha/2)

p1hat <- x/n1
p2hat <- y/n2

l <- (p1hat - p2hat) - z_alpha_2 * sqrt(p1hat*(1-p1hat)/n1 + p2hat*(1-p2hat)/n2)
u <- (p1hat - p2hat) + z_alpha_2 * sqrt(p1hat*(1-p1hat)/n1 + p2hat*(1-p2hat)/n2)

print(c(l, u))


# 예제 11 -------------------------------------------------------------------

alpha <- 0.05
z_alpha <- qnorm(1 - alpha)

phat <- (x + y) / (n1 + n2)

z <- (p1hat - p2hat) / (sqrt(phat*(1-phat))*sqrt(1/n1+1/n2))
z

print(c(z, z_alpha))

pvalue <- 1 - pnorm(z)
