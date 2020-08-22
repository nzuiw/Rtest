# sampletest --------------------------------------------------------------
# 1 -----------------------------------------------------------------------
# R보다는 손 풀이과정!
sigma <- 5
n <- 8
# s

(7*9)/25
(7*49)/25

pchisq(13.72, n-1) - pchisq(2.52, n-1)

# 2 -----------------------------------------------------------------------
mu <- 50
sigma <- sqrt(5)
n <- 6
N <- 10000

?rnorm
data <- rnorm(n*N, mu, sigma) #평균과 표준편차에 맞게 표본을 뽑는다 
data
data <- matrix(data, nrow = N, ncol = n) #6개씩 행렬 
data
S <- apply(data, 1, sd) # 1 = 행에 따라서
S
G <- S^2
G
hist(G, prob = T)
# 이론적인 분포..? 카이분포인가?
# 카이분포의 개념; 정규모집단으로부터 추출될 표본의 표본표준편차를 나타내기 때문 

# 이론적
x <- seq(0, 30, by = 0.5)
hist(G, breaks = x, prob = 'T')
lines(x, dchisq(x, n-1), col = 'red')


# 3 -----------------------------------------------------------------------

n <- 120
p <- 80/120
xbar <- n*p
s <- sqrt(n*p*(1-p))
s

alpha <- 0.05
z_alpha_2 <- qnorm(1 - alpha/2)

l <- xbar - z_alpha_2*(s/sqrt(n))
u <- xbar + z_alpha_2*(s/sqrt(n))

print(c(l, u))


# 4 -----------------------------------------------------------------------

data <- c(6.1, 4.8, 5.6, 3.9, 6.2, 5.9, 5.2, 5.7)
n <- length(data)
xbar <- mean(data)
s <- sd(data)
xbar
s

mu0 <- 5
alpha <- 0.05
t_alpha <- qt(1 - alpha, n-1)

t <- (xbar - mu0) / (s/sqrt(n)) #검정통계량 

print(c(t, t_alpha))
# 귀무가설을 기각할 수 없다

pvalue <- 1 -  pt(t, n-1)
pvalue

print(c(t, t_alpha, pvalue))
#p값 약 8%.. 에서 기각할 수 있다 


# 5 -----------------------------------------------------------------------


# 6 -----------------------------------------------------------------------

data <- c(13, 14, 16, 19, 18, 14, 12)
xbar <- mean(data)
s <- sd(data)
n <- length(data)
xbar
s

alpha <- 0.05
chi_1_alpha_2 <- qchisq(alpha/2, n-1)
chi_alpha_2 <- qchisq(1 - alpha/2, n-1)
print(c(chi_1_alpha_2, chi_alpha_2))

l <- s*sqrt((n-1)/chi_alpha_2)
u <- s*sqrt((n-1)/chi_1_alpha_2)

print(c(l, u))


# 7 -----------------------------------------------------------------------

# '모평균'에 대한 신뢰구간
n <- 28
l <- 21.84
u <- 32.56

alpha <- 0.1
t_alpha_2 <- qt(1 - alpha/2, n-1)

xbar <- (l+u)/2
s <- ((u-l)/2)*(sqrt(n)/t_alpha_2)
((u-l)/2)
(sqrt(n)/t_alpha_2)
xbar
s

# (1)
alpha <- 0.05
chi_1_alpha_2 <- qchisq(alpha/2, n-1)
chi_alpha_2 <- qchisq(1 - alpha/2, n-1)
print(c(chi_1_alpha_2, chi_alpha_2))

l <- s*sqrt((n-1)/chi_alpha_2)
u <- s*sqrt((n-1)/chi_1_alpha_2)
print(c(l,u))

# (2)
sigma0 <- 20
alpha <- 0.05

# 검정통계량 
chisq <- ((n-1)*s^2)/sigma0^2
chi_1_alpha <- qchisq(alpha, n-1)
print(c(chisq, chi_1_alpha))
# 기각할 수 없다 


# 8 -----------------------------------------------------------------------

n1 <- 80
n2 <- 120
xbar <- 78.4
ybar <- 82.3
s1 <- 9.1
s2 <- 8.5

alpha <- 0.05
delta0 <- 0

z <- ((xbar - ybar) - delta0) / sqrt(s1^2/n1 + s2^2/n2)
z_alpha_2 <- qnorm(1 - alpha/2)
print(c(z, z_alpha_2))
# 차이가 있다고 할 수 있다 


# 9 -----------------------------------------------------------------------

n1 <- 6
n2 <- 6
xbar <- 43.2
ybar <- 54.7
s1 <- 6.2
s2 <- 7.4

delta0 <- 0

s_p <- sqrt(((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2))
s_p
t <- ((xbar - ybar) - delta0) / s_p*sqrt(1/n1+1/n2)
t
pvalue <- pt(t, n1+n2-2)
pvalue


# Quiz 1 ------------------------------------------------------------------

# 1 -----------------------------------------------------------------------

# (a)
mu0 <- 60
n <- 9

# (b)
alpha <- 0.01
t_alpha <- qt(1 - alpha, n-1)

# (c)
data <- c(63, 78, 58, 62, 59, 66, 65, 57, 55)
n <- length(data)
xbar <- mean(data)
s <- sd(data)
xbar
s

t <- (xbar - mu0)/(s/sqrt(n))
print(c(t, t_alpha))

pvalue <- 1 - pt(t, n-1)
print(c(t, t_alpha, pvalue))


# 2 -----------------------------------------------------------------------


# 3 -----------------------------------------------------------------------

# (a)
n <- 5
xbar <- 60

4*4/25
4*100/25

pchisq(16, n-1) - pchisq(0.64, n-1)

# (b)
pt(2*sqrt(5), n-1) - pt(-2*sqrt(5), n-1)


# 4 -----------------------------------------------------------------------


