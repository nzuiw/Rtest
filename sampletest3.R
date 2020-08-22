# 4장 ----------------------------------------------------------------------

y1 <- c(10, 15, 8, 12, 15)
y2 <- c(14, 18, 21, 15)
y3 <- c(17, 16, 14, 15, 17, 15, 18)
y4 <- c(12, 15, 17, 15, 16, 15)

k <- 4
ni <- c( length(y1), length(y2), length(y3), length(y4))

yibar <- c(mean(y1), mean(y2), mean(y3), mean(y4))
yibar

y <- c(y1, y2, y3, y4)
ybar <- mean(y)
ybar

sst <- sum((y-ybar)^2)
sstr <- sum(ni*(yibar-ybar)^2)
sse <- sst - sstr

ss <- c(sstr, sse, sst) # 순서 주의 
df <- c(k-1, n-k, n-1)
ms <- ss/df
F0 <- ms[1]/ms[2]

# anova
anovatable <- data.frame("제곱합"=ss, "자유도"=df, "평균제곱"=ms, "F값" = c(F0,"",""))
rownames(anovatable) <- c("처리", "오차", "합계")
anovatable

alpha <- 0.05
f_alpha <- qf(1 - alpha, k-1, n-k)
print(c(F0, f_alpha))


# 5장 ----------------------------------------------------------------------

# ex 4 적합도검정 

o <- c(18, 55, 27)
p <- c(1/4, 1/2, 1/4)
n <- sum(o)
k <- length(o)
e <- n*p

d <- ((o-e)^2) / e
chisq0 <- sum(d)

alpha <- 0.05
chisq_alpha <- qchisq(1 - alpha, k-1)

print(c(chisq0, chisq_alpha))

pvalue <- 1 - pchisq(chisq0,k-1)
print(c(chisq0, chisq_alpha, pvalue))

# ex 5 동질성검정 

oa <- c(37, 24, 19)
ob <- c(17, 33, 20)

namelist <- list(c("A", "B"), c("양호", "보통", "불량"))
o <- matrix(c(oa, ob), nrow = 2, ncol = 3, byrow = T,  dimnames = namelist)
o

rowsum <- apply(o, 1, sum)
colsum <- apply(o, 2, sum)
e <- (rowsum %*% t(colsum) ) / sum(o)
e

d <- (o-e)^2 / e
chisq0 <- sum(d)

alpha <- 0.05
df <- (length(rowsum)-1)*(length(colsum)-1)
chisq_alpha <- qchisq(1 - alpha, df)

print(c(chisq0, chisq_alpha))

# ex 6 동질성검정

oa <- c(32, 268)
ob <- c(51, 199)
oc <- c(67, 233)
od <- c(83, 267)

namelist <- list(c("사무원", "교육자", "기업인", "상인"), c("알코올중독", "정상"))
o <- matrix(c(oa, ob, oc, od), nrow=4, ncol = 2, byrow = T, dimnames = namelist)
o

rowsum <- apply(o, 1, sum)
colsum <- apply(o, 2, sum)

e <- rowsum %*% t(colsum) / sum(o)
e
dimnames(e) <- namelist
e

d <- ((o-e)^2) / e
d
chisq0 <-sum(d)

alpha <- 0.05
df <- (length(rowsum) - 1) * (length(colsum) - 1)
chisq_alpha <- qchisq(1 - alpha, df)

print(c(chisq0, chisq_alpha))

pvalue <- 1 - pchisq(chisq0, df)
print(c(chisq0, chisq_alpha, pvalue))

# ex 8 독립성검정   

oa <- c(84, 16)
ob <- c(132, 18)

namelist <- list(c("처리함", "처리안함"), c("싹이틈", "싹이트지않음"))
o <- matrix(c(oa, ob), nrow = 2, ncol = 2, byrow = T, dimnames = namelist)
o

rowsum <- apply(o, 1, sum)
colsum <- apply(o, 2, sum)
e <- (rowsum %*% t(colsum)) / sum(o)
dimnames(e) <- namelist
e

d <- ((o-e)^2) / e
chisq0 <- sum(d)

alpha <- 0.05
df <- (length(rowsum)-1)*(length(colsum)-1)
chisq_alpha <- qchisq(1 - alpha, df)

pvalue <- pchisq(1 - alpha, df)

print(c(chisq0, chisq_alpha, pvalue))


# dice --------------------------------------------------------------------

dice <- c(3,3,5,3,6,2,4,1,5,3,4,5,6,2,3,1,2,5,6,3,2,5,4,6,1,2,5,3,6,1, 
          2,4,6,3,4,1,5,6,1,4,2,3,6,1,3,4,1,2,3,4,5,2,6,5,4,1,3,2,4,5, 
          2,3,4,2,1,6,5,1,3,4,5,3,2,4,5,1,6,3,4,5,2,6,4,3,5,1,2,6,2,3,
          4,5,6,1,2,3,1,5,1,4,6,2,5,3,4,5,1,2,3,5,6,2,3,4,1,6,5,1,4,2, 
          5,6,2,3,1,4,5,3,4,2,3,6,1,2,3,1,4,5,4,6,3,4,1,5,4,2,1,6,1,2,
          3,6,4,1,2,3,4,1,5,6,3,4,5,2,3,6,4,1,5,6,2,3,4,5,2,4,5,6,2,3, 
          1,5,4,3,4,2,6,5,1,2,4,5,6,2,3,1,5,6,3,4,1,6,5,1,4,2,6,5,3,4, 
          1,4,6,3,4,5,1,6,3,1,5,2,3,6,4,6,4,1,6,3,4,5,6,2,1,6,5,4,3,5, 
          1,1,4,2,5,3,4,1,6,3,1,6,5,3,5,2,6,1,2,6,4,1,3,5,1,2,6,3,4,2, 
          1,2,6,4,5,4,1,5,3,6,2,3,5,1,6,1,5,3,6,2,4,1,5,2,6,3,4,2,6,3, 
          1,3,2,1,6,3,5,2,6,3,5,2,6,5,3,6,5,3,1,4,2,6,4,5,3,5,2,6,1,2, 
          5,2,3,4,6,5,3,2,4,4)
# 동일하게 1/6 의 확률을 갖는 주사위인가?

# 1 적합도검정
o <- table(dice)
p <- c(1, 1, 1, 1, 1, 1) / 6
e <- p*sum(o)
d <- ((o-e)^2)/e
chisq0 <- sum(d)
pvalue <- 1 - pchisq(chisq0, 6-1)
pvalue

# 2
n <- length(dice)
dice_df <- data.frame(x1 = dice[1:n-1], x2 = dice[2:n])

o <- table(dice_df)
o

rowsum <- apply(o, 1, sum)
colsum <- apply(o, 2, sum)
r <- length(rowsum)
c <- length(colsum)
e <- (rowsum %*% t(colsum)) / sum(o)
d <- ((o-e)^2)/e
chisq0 <- sum(d)

alpha <- 0.05
chisq_alpha <- qchisq(1 - alpha, (r-1)*(c-1))
pvalue <- 1 - pchisq(chisq0, (r-1)*(c-1))
print(c(chisq0, chisq_alpha, pvalue))

