#reopen with encoding
# ex1 ---------------------------------------------------------------------

# ?? ?삁?젣1) ?븞寃쎌쓽 ?몴硫? ?넀?긽 諛⑹?瑜? ?쐞?빐 A, B, C, D?쓽 肄뷀똿泥섎━ ?썑 留덈え?룄 痢≪젙

# A	  10, 15,  8, 12, 15
# B	  14, 18, 21, 15
# C	  17, 16, 14, 15, 17, 15, 18
# D	  12, 15, 17, 15, 16, 15


y1 <- c(10, 15,  8, 12, 15)
y2 <- c(14, 18, 21, 15)
y3 <- c(17, 16, 14, 15, 17, 15, 18)
y4 <- c(12, 15, 17, 15, 16, 15)

k <- 4 

y1bar <- mean(y1)
y2bar <- mean(y2)
y3bar <- mean(y3)
y4bar <- mean(y4)

ni <- c(length(y1), length(y2), length(y3), length(y4))
yibar <- c(mean(y1), mean(y2), mean(y3), mean(y4))
n <- sum(ni)
y <- c(y1, y2, y3, y4)
ybar <- mean(y)

sst <- sum((y-ybar)^2)
# sst <- var(y)*(n-1) ?? 媛숇떎 
sstr <- sum(ni*(yibar-ybar)^2)
sse <- sum((ni-1)*c(var(y1), var(y2), var(y3), var(y4)))
sse <- sst - sstr

ss <- c(sstr, sse, sst)
Df <- c(k-1, n-k, n-1)
ms <- ss/Df
F0 <- ms[1]/ms[2]

# ANOVA
anovatable <- data.frame("제곱합"=ss, "자유도"=Df, "평균제곱"=ms, "F값"=c(F0,"","")) # ?몴?쁽?븷媛믩뱾?꽔湲? 
rownames(anovatable) <- c("처리", "오차", "합계")
print(anovatable)

# ex ----------------------------------------------------------------------
# ANOVA

y1 <- c(10, 15,  8, 12, 15)
y2 <- c(14, 18, 21, 15)
y3 <- c(17, 16, 14, 15, 17, 15, 18)
y4 <- c(12, 15, 17, 15, 16, 15)

ni <- c(length(y1), length(y2), length(y3), length(y4))
group <- rep(c('A', 'B', 'C', 'D'), ni)
data <- data.frame( '留덈え?룄' = c(y1, y2, y3, y4), '肄뷀똿' = group )
print(data)

fit <- lm(留덈え?룄 ~ 肄뷀똿, data)
# lm = linear model 留덈え?룄媛 肄뷀똿?쑝濡? ?뼱?뼸寃? ?꽕紐낅릺?뒗吏?
aTbl <- anova(fit)
print(aTbl)


# ex 3 --------------------------------------------------------------------

# F-遺꾪룷

alpha <- 0.05
f_alpha <- qf(1-alpha, k-1, n-k) # 遺꾩옄?쓽 ?옄?쑀?룄, 遺꾨え?쓽 ?옄?쑀?룄
print(f_alpha)

print(c(F0, f_alpha))
# H0?쓣 湲곌컖?븷 ?닔 ?엳?떎.

pvalue <- 1 - pf(F0, k-1, n-k)
pvalue


anovatable <- data.frame("?젣怨깊빀"=ss, "?옄?쑀?룄"=Df, "?룊洹좎젣怨?"=ms, "F媛?"=c(F0,"",""), "P媛?") # ?몴?쁽?븷媛믩뱾?꽔湲? 
rownames(anovatable) <- c("泥섎━", "?삤李?", "?빀怨?")
print(anovatable)
