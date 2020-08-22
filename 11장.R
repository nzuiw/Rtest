
# 그래프그리기 ------------------------------------------------------------------

# 표준정규분포

x <-  seq(-5, 5, length.out = 101)
x
y <- (1 / sqrt(2*pi))*exp(-x^2 / 2)
y

plot(x, y, col = 'red', type = 'l')

# t-분포

a <- 5
t <- factorial((a+1)/2 - 1)*(1+x^2/a)^(-(a+1)/2) / (sqrt(pi*a))*factorial(1/2 - 1)
t

plot(x, t, type = 'l')
lines(x, t)
?lines


# 예제 1 --------------------------------------------------------------------

qt(0.9, 2)
qt(0.9, 10)


# 예제 2 --------------------------------------------------------------------

qt(1 - 0.05, 9)


# 예제 3 --------------------------------------------------------------------

n <- 15
xbar <- 39.3
s <- 2.6
alpha <- 0.1
# t_alpha_2 <- qt(1 - alpha/2, n-1)
# d <- t_alpha_2 * 2/sqrt(n)

l <- xbar - qt(1 - alpha/2, n-1)*(s/sqrt(n))
l
u <- xbar + qt(1 - alpha/2, n-1)*(s/sqrt(n))
u

print(c(l, u))


# 예제 4 --------------------------------------------------------------------

n <- 10 #langth(data)
data <- c(175, 190, 215, 198, 184, 
          207, 210, 193, 196, 180)
xbar <- mean(data)
s <- sd(data)
mu0 <- 200

alpha <- 0.05

t0 <- (xbar - mu0) / (s / sqrt(n))
t0

talpha <- qt(1 - alpha, n-1)
talpha

pvalue <- pt(t0, n-1)
pvalue

print(c(t0, talpha, pvalue))


# 예제 5 --------------------------------------------------------------------

n <- 9
xbar <- 8.3
s <- 1.2
alpha <- 0.05

talpha <- qt(1 - alpha/2, n-1)
talpha

# 신뢰구간 구하기
l <- xbar - talpha*(s / sqrt(n))
u <- xbar + talpha*(s / sqrt(n))
print(c(l, u))

# 가설검정하기
alpha <- 0.05
mu0 <- 8.5

talpha <- qt(1 - alpha/2, n-1)
talpha

t0 <- (xbar - mu0) / (s/sqrt(n))
t0

print(c(talpha, t0))


# 예제 6 --------------------------------------------------------------------

?qchisq

qchisq(1 - 0.05, 17) #상위확률
qchisq(0.05, 17) #하위확률


# 예제 7 --------------------------------------------------------------------

n <- 10
s <- 0.4
alpha <- 0.1

# 신뢰구간
chisq_alpha_half <- qchisq(1 - alpha/2, n-1)
chisq_1_alpha_half <- qchisq(alpha/2, n-1)

u <- s * sqrt((n - 1)/chisq_1_alpha_half)
u
l <- s * sqrt((n - 1)/chisq_alpha_half)
l

print(c(l, u))


# 예제 8 --------------------------------------------------------------------

n <- 10
s <- 0.4
sigma0 <- 0.2
alpha <- 0.05

# 검정통계량 
chisq0 <- (n - 1) * s^2 / sigma0^2
chisq0

chisq_alpha <- qchisq(1-alpha, n-1)
chisq_alpha

print(c(chisq0, chisq_alpha))
# 카이제곱이 더 크다 
# H0 기각

#p-value

pvalue <- 1 - pchisq(chisq0, n-1)
pvalue
# alpha보다 작으니까 기각 가능! 


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
data
data <- matrix(data, nrow = trials, ncol = n)
data
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

# 비교: 값이 큰 차이 없이 나온다

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

# 비교: 히스토그램이 빨간색 선에 더 가깝게 나온다


# 연습 - 그래프그리기 -------------------------------------------------------------

# 자유도에 따른 정규분포와 t분포와의 비교

data <- seq(-4, 4, length.out = 101) # 101조각으로 나누겠다는 뜻
data

plot(data, dnorm(data, 0, 1), col = 'red', type = 'l', xlim = c(-4, 4), ylim = c(0, 0.4))
par(new = T)
plot(data, dt(data, 5), type = 'l', col = 'blue')
par(new = T)
plot(data, dt(data, 10), type = 'l', col = 'green')
par(new = T)
plot(data, dt(data, 30), type = 'l', col = 'orange')

# 양수일 때

a <- seq(0, 0.5, length.out = 101) # 101조각으로 나누겠다는 뜻
a


plot(a, qt(1 - a, 5), type = 'l', col = 'blue', xlim = c(0, 0.5))
par(new = T)
plot(a, qt(1 - a, 10), type = 'l', col = 'red', xlim = c(0, 0.5))
par(new = T)
plot(a, qt(1 - a, 30), type = 'l', col = 'green', xlim = c(0, 0.5))
par(new = T)
