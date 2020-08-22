
# 예제 2 --------------------------------------------------------------------

x <- c(3, 3, 4, 5, 6, 6, 7, 8, 8, 9)
y <- c(9, 5, 12, 9, 14, 16, 22, 18, 24, 22)

xbar <- mean(x)
ybar <- mean(y)
n <- length(x)

s_xx <- sum(x^2) - n*(xbar)^2 
# sum((x-xbar)^2) 와 같다
# var(x)*(n-1) 와 같다 
s_yy <- sum(y^2) - n*(ybar)^2
s_xy <- sum(x*y) - n*xbar*ybar

beta1_hat <- s_xy / s_xx
beta0_hat <- ybar - beta1_hat*xbar
print(c(beta0_hat, beta1_hat))

plot(x, y)
x2 <- seq(0, 10, 0.1)
y2 <- beta0_hat + beta1_hat * x2
lines(x2, y2, col = 'blue')
# 그림에서 관측값은 점, 추정치는 직선 

# 잔차 계산

yhat <- beta0_hat + beta1_hat * x
e <- y - yhat
sum(e) #값은 거의 0
sum(x*e) #값은 거의 0

sse <- sum(e^2)
mse <- sse/(n-2)
print(c(sse, mse))


# 예제 3 ----------------------------------------------------------------------


# 예제 4 --------------------------------------------------------------------

# t0
x <- c(3, 3, 4, 5, 6, 6, 7, 8, 8, 9)
y <- c(9, 5, 12, 9, 14, 16, 22, 18, 24, 22)

n <- length(x)
xbar <- mean(x)
ybar <- mean(y)
s_xx <- sum(x^2) - n*(xbar)^2 
# sum((x-xbar)^2) 와 같다
# var(x)*(n-1) 와 같다 
s_yy <- sum(y^2) - n*(ybar)^2
s_xy <- sum(x*y) - n*xbar*ybar

beta1_hat <- s_xy/s_xx
beta0_hat <- ybar - beta1_hat * xbar

SEE <- s_yy - s_xy^2/s_xx
s <- sqrt(SEE/(n-2))

alpha <- 0.05
beta10 <- 0

t0 <- (beta1_hat - beta10) / (s/sqrt(s_xx))
t_alpha <- qt(1 - alpha, n-2)
print(c(t0, t_alpha))


# 예제 5 --------------------------------------------------------------------

alpha <- 0.05
t_alpha_2 <- qt(1 - alpha/2, n-2)

l <- beta0_hat - t_alpha_2 * s* sqrt((1/n) + (xbar^2/s_xx))
u <- beta0_hat + t_alpha_2 * s* sqrt((1/n) + (xbar^2/s_xx))
print(c(l, u))

beta00 <- 0
t <- (beta0_hat - beta00) / (s* sqrt((1/n) + (xbar^2/s_xx)))
t
print(c(t, t_alpha_2))

# H0을 기각할 수 없다 
# 신뢰구간안에 검정통계량이 포함되기 때문 
# t, t,_alpha_2 를 비교한 값이 기각역에 포함되지 않기 때문 



# 예제 6 --------------------------------------------------------------------

# t0
x <- c(3, 3, 4, 5, 6, 6, 7, 8, 8, 9)
y <- c(9, 5, 12, 9, 14, 16, 22, 18, 24, 22)

n <- length(x)
xbar <- mean(x)
ybar <- mean(y)
s_xx <- sum(x^2) - n*(xbar)^2 
# sum((x-xbar)^2) 와 같다
# var(x)*(n-1) 와 같다 
s_yy <- sum(y^2) - n*(ybar)^2
s_xy <- sum(x*y) - n*xbar*ybar

beta1_hat <- s_xy/s_xx
beta0_hat <- ybar - beta1_hat * xbar

SSE <- s_yy - s_xy^2/s_xx
s <- sqrt(SSE/(n-2))

# xstar <- 6
xstar <- 9.5
alpha <- 0.05
t_alpha_2 <- qt(1 - alpha/2, n-2)

l<- (beta0_hat + beta1_hat*xstar) - t_alpha_2 * s * sqrt(1/n + (xstar - xbar)^2/ s_xx)
u<- (beta0_hat + beta1_hat*xstar) + t_alpha_2 * s * sqrt(1/n + (xstar - xbar)^2/ s_xx)
print(c(l, u))


# 예제 7 --------------------------------------------------------------------

xstar <- 6
alpha <- 0.05
t_alpha_2 <- qt(1 - alpha/2, n-2)

l<- (beta0_hat + beta1_hat*xstar) - t_alpha_2 * s * sqrt(1 + 1/n + (xstar - xbar)^2/ s_xx)
u<- (beta0_hat + beta1_hat*xstar) + t_alpha_2 * s * sqrt(1 + 1/n + (xstar - xbar)^2/ s_xx)
print(c(l, u))

# 조금 더 넓은 범위



# 예제 8 --------------------------------------------------------------------
# 결정계수 R^2 구하기 

x <- c(3, 3, 4, 5, 6, 6, 7, 8, 8, 9)
y <- c(9, 5, 12, 9, 14, 16, 22, 18, 24, 22)

xbar <- mean(x)
ybar <- mean(y)
n <- length(x)

s_xx <- sum(x^2) - n*(xbar)^2 
# sum((x-xbar)^2) 와 같다
# var(x)*(n-1) 와 같다 
s_yy <- sum(y^2) - n*(ybar)^2
s_xy <- sum(x*y) - n*xbar*ybar

SST <- s_yy
SSR <- s_xy^2 / s_xx

R_2 <- SSR/SST
R_2
# 전체의 약 82%가 단순선형회귀모형으로 설명됨 


# 산점도 그리기 -----------------------------------------------------------------

#잔차와 예측값의 산점도 
e
yhat
plot(yhat, e)
plot(x, e)

