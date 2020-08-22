# 15장 연습문제 ----------------------------------------------------------------
# 2.1 ---------------------------------------------------------------------

# H0 : 주사위가 1부터 6의 눈이 나올 확률이 같다. 
o <- c(38, 61, 54, 65, 55, 37)
k <- length(o)
n <- sum(o)
p <- 1/6
e <- n*p
d <- ((o-e)^2)/e
chisq0 <- sum(d)

alpha <- 0.05
chisq_alpha <- qchisq(1 - alpha, k-1)
pvalue <- 1 - pchisq(chisq0, k-1)

print(c(chisq0, chisq_alpha, pvalue))
# H0 기각할 수 있다.
# 따라서 주사위의 눈이 나올 확률이 같다고 볼 수 없다.


# 2.7 ---------------------------------------------------------------------

# H0 : 각 요일에 태어날 확률은 같다.
o <- c(52.09, 54.46, 52.68, 51.68, 53.83, 47.21, 44.36)
n <- sum(o)
k <- length(o)
p <-1/7
e <- n*p
d <- ((o-e)^2)/e
chisq0 <- sum(d)

alpha <- 0.01
chisq_alpha <- qchisq(1 - alpha, k-1)
pvalue <- 1 - pchisq(chisq0, k-1)

print(c(chisq0, chisq_alpha, pvalue))
# H0을 기각할 수 없다.
# 따라서 각 요일에 태어날 확률은 같다고 할 수 있다.


# 2.9 ---------------------------------------------------------------------

# H0: 약효 지속시간이 정규분포(60, 15^2)을 따른다.
o <- c(16, 28, 36, 20)
n <- sum(o)
k <- length(o)

p1 <- pnorm(48, 60, 15)
p2 <- pnorm(60, 60, 15) - p1
p3 <- pnorm(72, 60, 15) - p1 -p2
p4 <- 1 - pnorm(72, 60, 15)
p <- c(p1, p2, p3, p4)

e <- n*p
d <- ((o-e)^2)/e
chisq0 <- sum(d)

alpha <- 0.05
chisq_alpha <- qchisq(1 - alpha, k-1)
pvalue <- 1 - pchisq(chisq0, k-1)

print(c(chisq0, chisq_alpha, pvalue))


# 2.11 --------------------------------------------------------------------

# H0: 수컷의 수가 이항분포를 따른다.

# 1
p <- dbinom(c(0, 1, 2, 3), 3, 0.4)

o <- c(19, 32, 22, 7)
n <- sum(o)
k <- length(o)
e <- n*p
d <- ((o-e)^2)/e
chisq0 <- sum(d)

# 2
alpha <- 0.05
chisq_alpha <- qchisq(1 - alpha, k-2)
pvalue <- 1 - pchisq(chisq0, k-2)

print(c(chisq0, chisq_alpha, pvalue))
# H0을 기각할 수 없다.
# 수컷의 수는 이항분포를 따른다고 할 수 있다.


# 3.1 ---------------------------------------------------------------------

# 1
# H0: 손상된 잎의 비율은 식물의 종류에 따라 차이가 없다.

# 2
oa <- c(32, 8)
ob <- c(28, 12)
oc <- c(19, 21)

namelist <- list(c("양상추", "시금치", "토마토"), c("심각", "없거나심각하지않다"))
o <- matrix(c(oa, ob, oc), nrow = 3, ncol = 2, byrow = T, dimnames = namelist)
o
n <- sum(o)

rowsum <- apply(o, 1, sum)
colsum <- apply(o, 2, sum)
r <- length(rowsum)
c <- length(colsum)
df <- (r-1)*(c-1)

e <- (rowsum%*%t(colsum))/n
d <- ((o-e)^2)/e
chisq0 <- sum(d)

alpha <- 0.1
chisq_alpha <- qchisq(1 - alpha, df)
pvalue <- 1 - pchisq(chisq0, df)
print(c(chisq0, chisq_alpha, pvalue))
# H0을 기각한다.
# 잎의 비율이 식물의 종류에 따라 차이가 없다고 볼 수 없다.

# 3
# 신뢰구간
alpha <- 0.05
z_alpha_2 <-qnorm(1 - alpha/2)

p1 <- 32/40
l1 <- p1 - z_alpha_2 * sqrt(p1*(1-p1)/40)
u1 <- p1 + z_alpha_2 * sqrt(p1*(1-p1)/40)
print(c(l1, u1))  
  
p2 <- 28/40
l2 <- p2 - z_alpha_2 * sqrt(p2*(1-p2)/40)
u2 <- p2 + z_alpha_2 * sqrt(p2*(1-p2)/40)
print(c(l2, u2))  

p3 <- 19/40
l3 <- p3 - z_alpha_2 * sqrt(p3*(1-p3)/40)
u3 <- p3 + z_alpha_2 * sqrt(p3*(1-p3)/40)
print(c(l3, u3))  
  
  
# 3.5 ---------------------------------------------------------------------

# 1
# H0: 살충제 간에 차이가 없다. 
oa <- c(58, 57)
ob <- c(43, 77)
oc <- c(56, 42)
od <- c(45, 75)

o <- matrix(c(oa, ob, oc, od), nrow = 4, ncol = 2, byrow = T)
o
n <- sum(o)

rowsum <- apply(o, 1, sum)
colsum <- apply(o, 2, sum)
r <- length(rowsum)
c <- length(colsum)
df <- (r-1)*(c-1)

e <- (rowsum %*% t(colsum)) / n
e
d <- ((o-e)^2)/e
chisq0 <- sum(d)

alpha <- 0.1
chisq_alpha <- qchisq(1 - alpha, df)
pvalue <- 1 - pchisq(1 - alpha,df)

print(c(chisq0, chisq_alpha, pvalue))
# H0을 기각한다.
# 살충제 간에 차이가 없다고 말할 수 없다.

# 2
alpha <- 0.05
z_alpha_2 <- qnorm(1 - alpha/2)

pa <- 58/115
l1 <- pa - z_alpha_2 * sqrt(pa*(1-pa)/115)
u1 <- pa + z_alpha_2 * sqrt(pa*(1-pa)/115)
print(c(l1, u1))

pb <- 43/120
l2 <- pb - z_alpha_2 * sqrt(pb*(1-pb)/120)
u2 <- pb + z_alpha_2 * sqrt(pb*(1-pb)/120)
print(c(l2, u2))

pc <- 56/98
l3 <- pc - z_alpha_2 * sqrt(pc*(1-pc)/98)
u3 <- pc + z_alpha_2 * sqrt(pc*(1-pc)/98)
print(c(l3, u3))

pd <- 45/120
l4 <- pd - z_alpha_2 * sqrt(pd*(1-pd)/120)
u4 <- pd + z_alpha_2 * sqrt(pd*(1-pd)/120)
print(c(l4, u4))



# 3.6 ---------------------------------------------------------------------

# 1
oa <- c(58, 57)
oc <- c(56, 42)
o <- matrix(c(oa, oc), nrow = 2, ncol = 2, byrow = T)
o

n <- sum(o)
rowsum <- apply(o, 1, sum)
colsum <- apply(o, 2, sum)
r <- length(rowsum)
c <- length(colsum)
df <- (r-1)*(c-1)

e <- (rowsum %*% t(colsum))/n
d <- ((o-e)^2)/e
chisq0 <- sum(d)

alpha <- 0.05
chisq_alpha <- qchisq(1 - alpha, df)
pvalue <- 1 - pchisq(chisq0, df)

print(c(chisq0, chisq_alpha, pvalue))

# 2
pa <- 58/115
pc <- 56/98
p <- (58+56)/(115+98)

z0 <- (pa-pc)/sqrt(p*(1-p)*(1/115+1/98))

alpha <- 0.05
z_alpha_2 <- qnorm(1 - alpha/2)
pvalue <- 2*pnorm(z0)
print(c(z0, z_alpha_2, pvalue))


# 3.7 ---------------------------------------------------------------------

# H0: 각 노인집단에서 캄슘의 변화에 차이가 없다.
oa <- c(38, 15, 7)
ob <- c(22, 32, 16)
oc <- c(15, 30, 25)

o <- matrix(c(oa, ob, oc), nrow = 3, ncol = 3, byrow = T)
n <- sum(o)
k <- length(o)

rowsum <- apply(o, 1, sum)
colsum <- apply(o, 2, sum)
r <- length(rowsum)
c <- length(colsum)
df <- (r-1)*(c-1)

e <- (rowsum %*% t(colsum))/n
d <- ((o-e)^2)/e
chisq0 <- sum(d)

alpha <- 0.05
chisq_alpha <- qchisq(1 - alpha, df)
pvalue <- 1 - pchisq(chisq0, df)

print(c(chisq0, chisq_alpha, pvalue))


# 4.1 ---------------------------------------------------------------------

# H0: 본인의 직접 신청 여부와 수령액 증감 여부가 관계가 있다고 할 수 없다.

oa <- c(59, 108, 17)
ob <- c(70, 63, 3)
o <- matrix(c(oa, ob, oc), nrow = 2, ncol = 3, byrow = T)
n <- sum(o)
k <- length(o)

rowsum <- apply(o, 1, sum)
colsum <- apply(o, 2, sum)
r <- length(rowsum)
c <- length(colsum)
df <- (r-1)*(c-1)

e <- (rowsum %*% t(colsum)) /n
d <- ((o-e)^2)/e
chisq0 <- sum(d)

alpha <- 0.05
chisq_alpha <- qchisq(1-alpha, df)
pvalue <- 1 - pchisq(chisq0, df)

print(c(chisq0, chisq_alpha, pvalue))
# H0을 기각할 수 있다.
# 관계가 있다고 할 수 있다.

# 4.5 ---------------------------------------------------------------------

# H0: 출신지와 검정올리브의 선호여부는 관계가 없다.

oa <- c(65, 118)
ob <- c(59, 135)
oc <- c(48, 90)
od <- c(43, 92)
o <- matrix(c(oa, ob, oc, od), nrow = 4, ncol = 2, byrow = T)
n <- sum(o)
k <- length(o)

rowsum <- apply(o, 1, sum)
colsum <- apply(o, 2, sum)
r <- length(rowsum)
c <- length(colsum)
df <- (r-1)*(c-1)

e <- (rowsum %*% t(colsum))/n
d <- ((o-e)^2)/e
chisq0 <- sum(d)

alpha <- 0.05
chisq_alpha <- qchisq(1 - alpha, df)
pvalue <- 1 - pchisq(chisq0, df)

print(c(chisq0, chisq_alpha, pvalue))
# H0을 기각할 수 없다.
# 선호여부는 관계가 없다.