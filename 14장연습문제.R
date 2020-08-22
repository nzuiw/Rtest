# 14장 연습문제 -----------------------------------------------------------------
# 2.1 ---------------------------------------------------------------------

y1 <- c(6, 10)
y2 <- c(9, 5)
y3 <- c(9, 7)
y4 <- c(4, 6)

ni <- c(length(y1), length(y2), length(y3), length(y4))
yibar <- c(mean(y1), mean(y2), mean(y3), mean(y4))
n <- sum(ni)
y <- c(y1, y2, y3, y4)
ybar <- mean(y)
k <- 4

sst <- sum((y-ybar)^2)
sstr <-sum(ni*(yibar-ybar)^2)
sse <- sst - sstr

ss <- c(sstr, sse, sst)
df <- c(k-1, n-k, n-1)
ms <- ss/df
f0 <- ms[1]/ms[2]

# anova
anovatable <- data.frame("제곱합"=ss, "자유도"=df, "평균제곱"=ms, "F값"=c(f0,"",""))
anovatable
rownames(anovatable) <- c("처리", "오차", "합계")
anovatable


# 2.3 ---------------------------------------------------------------------

y1 <- c(5, 3, 2, 2)
y2 <- c(5, 0, 1)
y3 <- c(2, 1, 0, 1)

k <- 3
ni <- c(length(y1), length(y2), length(y3))
n <- sum(ni)
y <- c(y1, y2, y3)
yibar <- c(mean(y1), mean(y2), mean(y3))
ybar <- mean(y)

sst <- sum((y-ybar)^2)
sstr <- sum(ni*(yibar-ybar)^2)
sse <- sst - sstr

ss <- c(sstr, sse, sst)
df <- c(k-1, n-k, n-1)
ms <- ss/df
f0 <- ms[1]/ms[2]

anovatable <- data.frame("제곱합" = ss, "자유도" = df, "평균제곱" = ms, "f값" = c(f0,"",""))
anovatable
rownames(anovatable) <- c("처리", "오차", "총")
anovatable


# 2.7 ---------------------------------------------------------------------

y1bar <- 81.06
s1 <- 17.05
n1 <- 32
y2bar <- 78.56
s2 <- 15.43
n2 <- 16
y3bar <- 87.81
s3 <- 14.36
n3 <- 16

s <- c(s1, s2, s3)

k <- 3
ni <- c(n1, n2, n3)
n <- sum(ni)
# y <- 
yibar <- c(y1bar, y2bar, y3bar)
ybar <- (y1bar*n1 + y2bar*n2 + y3bar*n3) / (n1+n2+n3)

sstr <- sum(ni*(yibar-ybar)^2)
sse <- sum(s^2 * (ni-1))
sst <- sstr + sse

ss <- c(sstr, sse, sst)
df <- c(k-1, n-k, n-1)
ms <- ss/df
f0 <- ms[1]/ms[2]

anovatable <- data.frame("제곱합"=ss, "자유도"=df, "평균제곱"=ms, "F값"=c(f0,"",""))
rownames(anovatable) <- c("처리", "오차", "총")
anovatable


# 3.1 ---------------------------------------------------------------------

qf(1 - 0.05, 5, 10)
qf(1 - 0.05, 10, 5)


# 3.3 ---------------------------------------------------------------------

alpha <- 0.1
f_alpha <- qf(1 - alpha, 5, 20)

ss <- c(104, 109)
df <- c(5, 20)
ms <- ss/df
f0 <- ms[1]/ms[2]

pvalue <- 1 - pf(f0,5, 20)
print(c(f0, f_alpha, pvalue))


# 3.5 ---------------------------------------------------------------------

y1 <- c(6, 10)
y2 <- c(9, 5)
y3 <- c(9, 7)
y4 <- c(4, 6)

k <- 4
ni <- c(length(y1), length(y2), length(y3), length(y4))
n <- sum(ni)
y <- c(y1, y2, y3, y4)
yibar <- c(mean(y1), mean(y2), mean(y3), mean(y4))
ybar <- mean(y)

sst <- sum((y-ybar)^2)
sstr <- sum(ni*(yibar-ybar)^2)
sse <- sst - sstr

ss <- c(sstr, sse, sst)
df <- c(k-1, n-k, n-1)
ms <- ss/df
f0 <- ms[1]/ms[2]

alpha <- 0.05
f_alpha <- qf(1 - alpha, k-1, n-k)
pvalue <- 1 - pf(f0, k-1, n-k)
print(c(f0, f_alpha, pvalue))


# 3.9 ---------------------------------------------------------------------

# H0: 평균들이 동일하다. 
y1bar <- 81.06
s1 <- 17.05
n1 <- 32
y2bar <- 78.56
s2 <- 15.43
n2 <- 16
y3bar <- 87.81
s3 <- 14.36
n3 <- 16

k <- 3
ni <- c(n1, n2, n3)
n <- sum(ni)
# y
yibar <- c(y1bar, y2bar, y3bar)
ybar <- (y1bar*n1 + y2bar*n2 + y3bar*n3) / (n1+n2+n3)

s <- c(s1, s2, s3)
sstr <- sum(ni*(yibar - ybar)^2)
sse <- sum(s^2 * (ni-1))
sst <- sstr+sse

ss <- c(sstr, sse, sst)
df <- c(k-1, n-k, n-1)
ms <- ss/df
f0 <- ms[1]/ms[2]

alpha <- 0.05
f_alpha <- qf(1 - alpha, k-1, n-k)
pvalue <- pf(f0, k-1, n-k)
print(c(f0, f_alpha, pvalue))
# H0을 기각할 수 없다. 
# 평균들이 비슷하다. 
