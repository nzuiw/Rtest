# 3.1 ---------------------------------------------------------------------

# 1

x <- c(1, 2, 3, 4,5)
y <- c(0.9, 2.1, 2.5, 3.3, 3.8)

plot(x,y)

# 3
xbar <- mean(x)
ybar <- mean(y)
n <- length(x)

s_xx <- sum((x-xbar)^2)
s_xy <- sum((x-xbar)*(y-ybar))
s_yy <- sum((y-ybar)^2)

beta1_hat <- s_xy/s_xx
beta0_hat <- ybar - beta1_hat*xbar
print(c(beta0_hat, beta1_hat))

#직선그리기
yhat <- beta0_hat + beta1_hat*x
lines(x, yhat, col = 'red') 

# 3.7 ---------------------------------------------------------------------

# 1 
# 잔차 구하기
yhat <- beta0_hat + beta1_hat*x
e <- y - yhat
sum(e)

#2 
# 잔차제곱합 SSE

# 잔차의 제곱들을 더해서 계산
SSE <- sum(e^2)
# 공식 SSE = ... 이용해서 계산
SSE <- s_yy - (s_xy^2)/s_xx

# 3 

s <- SSE/(n-2)
s


# 3.9 ---------------------------------------------------------------------

n <- 18
xbar <- 1.2
ybar <- 5.1
s_xx <- 14.10
s_xy <- 2.31
s_yy <- 2.01

# 1
# 최적으로 적합하는 직선식

beta1_hat <- s_xy/s_xx
beta0_hat <- ybar - beta1_hat*xbar


# 2
# 잔차제곱합 SSE

yhat <- beta0_hat + beta1_hat*x
e <- y - yhat
e

sum(e^2)
# 틀림

SSE <- s_yy - s_xy^2/s_xx
SSE

# 3
# 오차분산 시그마^2 추정

MSE <- SSE/(n-2)
MSE


# 3.13 --------------------------------------------------------------------


