
# ex1 ---------------------------------------------------------------------

# A, B, C 
# 1:2:1
# 18, 55, 27


# ex2 ---------------------------------------------------------------------
# A : 37, 24, 19
# B : 17, 33, 20


# ex 4 --------------------------------------------------------------------
# A, B, C 
# 1:2:1
# 18, 55, 27

o <- c(18, 55, 27) # ??????
p <- c(1/4, 1/2, 1/4)
n <- sum(o)
k <- length(o)
e <- p*n

d <- (o-e)^2 / e
chisq0 <- sum(d)

alpha <- 0.05
chisq_alpha <- qchisq(1 - alpha, k-1)

print(c(chisq0, chisq_alpha))
# H0�� ?Ⱒ?? ?? ????. 

pvalue <- 1 - pchisq(chisq0, k-1)
pvalue

# ???̺????? ???? 
m <- matrix(c(o, e, d), nrow = 3, byrow = T,   # ?? ??��?? ?Է??ϰڴٴ? ??~
            dimnames = list(c("O","E","d"), c("A","B","C")))  # ????Ʈ?? ??�� 
m

s <- apply(m, 1, sum)
cbind(m, s) # m?? s?? ???? ???δ?


# ex5 -----------------------------------------------------------------------
# ???? ?̿??Ͽ? ???? ?????? ?ϱ?
# [80
# 70]   #[54 57 39]

a <- c(80, 70)
b <- c(54, 57, 39)
a %*% t(b) # ?

# ex2 ?ڷ?
# A : 37, 24, 19
# B : 17, 33, 20

oA <- c(37, 24, 19)
oB <- c(17, 33, 20)

namelist <- list(c("A", "B"), c("??ȣ", "????", "?ҷ?"))
o <- matrix(c(oA, oB), nrow = 2, ncol = 3, byrow = T, dimnames = namelist)
print(o)

rowsum <- apply(o, 1, sum) # ?? ??
colsum <- apply(o, 2, sum) # ?? ??
e <- (rowsum%*%t(colsum))/sum(o)
e

dimnames(e) <- namelist
e

d <- (o-e)^2 /e
d

chisq0 <- sum(d)
chisq0

alpha <- 0.05
df <- (length(rowsum) - 1) * (length(colsum) - 1)
chisq_alpha <- qchisq(1 - alpha, df)

print(c(chisq0, chisq_alpha))
# H0?? ?Ⱒ?? ?? ?ִ?

pvalue <- 1 - pchisq(chisq0, df)
pvalue

# ex6 ---------------------------------------------------------------------

# 32	268
# 51	199
# 67	233
# 83	267

oA <- c(32, 268)
oB <- c(51, 199)
oC <- c(67, 233)
oD <- c(83, 267)

o <- matrix(c(oA, oB, oC, oD), nrow = 4, ncol = 2, byrow = T)
namelist <- list(c("?繫??", "??��??", "??????", "????"), c("???ڿ? ?ߵ?", "��??"))
o <- matrix(c(oA, oB, oC, oD), nrow = 4, ncol = 2, byrow = T, dimnames = namelist)
o

rowsum <- apply(o, 1, sum)
colsum <- apply(o, 2, sum)
e <- (rowsum %*% t(colsum)) / sum(o)
e

dimnames(e) <- namelist
e

d <- (o-e)^2 / e
d

alpha <- 0.05
df <- (length(rowsum) - 1) * (length(colsum) - 1)
chisq_alpha <- qchisq(1 - alpha, df)

chisq0 <- sum(d)

print(c(chisq0, chisq_alpha))
pvalue <- 1 - pchisq(chisq0, df)
pvalue


# ex7 ---------------------------------------------------------------------

# 84	16
# 132	18

o <- matrix (c(84, 16, 132, 18), nrow = 2, byrow = T)
o

# ��?? ????��?? ?ߴµ? ?? ?ȵǴ? ?ŤӤ?./?

rowsum <- apply(o, 1, sum)
colsum <- apply(o, 2, sum)
e <- (rowsum %*% t(colsum)) / sum(o)
e

#dimnames(e) <- nameslists
e

d <- (o-e)^2 / e
d

alpha <- 0.05
df <- (length(rowsum) - 1) * (length(colsum) - 1)
chisq_alpha <- qchisq(1 - alpha, df)

chisq0 <- sum(d)

print(c(chisq0, chisq_alpha))
pvalue <- 1 - pchisq(chisq0, df)
pvalue

# ex8 ---------------------------------------------------------------------

# 378, 237, 26
# 388, 196, 25

namelist <- list(c("??", "??"), c("????", "????", "????"))
o <- matrix(c(378, 237, 26, 388, 196, 25),nrow = 2, ncol = 3, byrow = TRUE, dimnames = namelist)
o

rowsum <- apply(o, 1, sum)
colsum <- apply(o, 2, sum)
r <- length(rowsum)
c <- length(colsum)

e <- (rowsum %*% t(colsum)) / sum(o)
d <- (o-e)^2 / e
chisq0 <- sum(d)

alpha <- 0.05
chisq_alpha <- qchisq(1 - alpha, (r-1)*(c-1))

print(c(chisq0, chisq_alpha))

pvalue <- 1 - pchisq(chisq0, (r-1)*(c-1))
pvalue

print(c(chisq0, chisq_alpha, pvalue))


# ???? 5?? ???��?�� --------------------------------------------------------------
# 2.1 ---------------------------------------------------------------------
# ?ֻ?��?? 310?? ??��?? ?????? ?ڷ? ?̿??ؼ? ?ֻ?��?? 1~6 Ȯ?? ??��?? 5% ??��



# 2.9 ---------------------------------------------------------------------
# ~N(60, 15^2)
# 100?? ??, 48?ð? ?̸?;16?? , 48?̻? 60?̸?;28?? , 60?̻? 72?̸?;36??, ??????;20?? 
# H0 : p1 = p10, p2 = , p3 = , p4 = 

p10 <- pnorm(48, mean = 60, sd = 15)
p20 <- pnorm(60, mean = 60, sd = 15) - p10
p30 <- pnorm(72, mean = 60, sd = 15) - pnorm(60, mean = 60, sd = 15)
p40 <- 1 - pnorm(72, mean = 60, sd = 15)
# a <- c(0, pnorm(c(48, 60, 72), 60, 15), 1)
# p <- a[2:5] - a[1:4]

o <- c(16, 28, 36, 20)
p <- c(p10, p20, p30, p40)
n <- sum(o)
k <- length(o)

e <- n*p
d <- ((o-e)^2) / e
chisq0 <- sum(d)

alpha <- 0.05
chisq_alpha <- qchisq(1 - alpha, k-1)

pvalue <- 1 - pchisq(chisq0, k-1)

print(c(chisq0, chisq_alpha, pvalue))
# ?־??? ??ȿ ???? ?ð??? ��?Ժ????? ??????.


# 2.11 --------------------------------------------------------------------
# ?䳢 80?????? 3?????? ?????? ???Ҵ?
# ?????? ???? ???? ?з? (????ǥ)
# ???? ?? 0   1  2  3
# ????    19 32 22  7  80
# (1) n=3, p=0.4 ?? ???׺??? ... 
# (2) ???׺????? ?????°?? 

# 1
n <- 3
p0 <- 0.4
p <- dbinom(c(0, 1, 2, 3), n, 0.4)

o <- c(19, 32, 22, 7)
e <- sum(o) * p
d <- ((o-e)^2) / e
chisq0 <- sum(d)

alpha <- 0.05
k <- length(o)
chisq_alpha <- qchisq(1 - alpha, k-1)

pvalue <- 1 - pchisq(chisq0, k-1)

print(c(chisq0, chisq_alpha, pvalue))


# 2
# ???׺????? ?????°?? -> ?? H0?? ?Ǵ? ??��
sum(o*c(0,1,2,3))/80  
(sum(o*c(0,1,2,3))/80)/3 
pvalue <- 1 - pchisq(chisq0, k-2)
# ?͹?????�� ?Ⱒ?? ?? ????. ??? 