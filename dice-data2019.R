# dice data ---------------------------------------------------------------

dice <- c(3,3,5,3,6,2,4,1,5,3,
          4,5,6,2,3,1,2,5,6,3, 
          2,5,4,6,1,2,5,3,6,1, 
          2,4,6,3,4,1,5,6,1,4, 
          2,3,6,1,3,4,1,2,3,4, 
          5,2,6,5,4,1,3,2,4,5, 
          2,3,4,2,1,6,5,1,3,4, 
          5,3,2,4,5,1,6,3,4,5, 
          2,6,4,3,5,1,2,6,2,3, 
          4,5,6,1,2,3,1,5,1,4, 
          6,2,5,3,4,5,1,2,3,5, 
          6,2,3,4,1,6,5,1,4,2, 
          5,6,2,3,1,4,5,3,4,2, 
          3,6,1,2,3,1,4,5,4,6, 
          3,4,1,5,4,2,1,6,1,2, 
          3,6,4,1,2,3,4,1,5,6, 
          3,4,5,2,3,6,4,1,5,6, 
          2,3,4,5,2,4,5,6,2,3, 
          1,5,4,3,4,2,6,5,1,2, 
          4,5,6,2,3,1,5,6,3,4, 
          1,6,5,1,4,2,6,5,3,4, 
          1,4,6,3,4,5,1,6,3,1, 
          5,2,3,6,4,6,4,1,6,3, 
          4,5,6,2,1,6,5,4,3,5, 
          1,1,4,2,5,3,4,1,6,3, 
          1,6,5,3,5,2,6,1,2,6, 
          4,1,3,5,1,2,6,3,4,2, 
          1,2,6,4,5,4,1,5,3,6, 
          2,3,5,1,6,1,5,3,6,2, 
          4,1,5,2,6,3,4,2,6,3, 
          1,3,2,1,6,3,5,2,6,3, 
          5,2,6,5,3,6,5,3,1,4, 
          2,6,4,5,3,5,2,6,1,2, 
          5,2,3,4,6,5,3,2,4,4)
# ?????ϰ? 1/6 ?? Ȯ??�� ???? ?ֻ?��?ΰ??



# (1) ---------------------------------------------------------------------
# ???յ? ??��
# H0 : p1 = p2 = ... = p6 = 1/6
o <- table(dice)
p <- c(1, 1, 1, 1, 1, 1) / 6
e <- p * sum(o)
d <- (o-e)^2 / e

chisq0 <- sum(d)
pvalue <- 1 - pchisq(chisq0, 5)
print(pvalue)
# ?Ⱒ�� ?? ???ɼ??? ????.


# 2 ----------------------------------------
# ?ֻ?�� ?? (X, Y)?? ???????ΰ??
n <- length(dice)
dice_df <- data.frame(x1 = dice[1:n-1], x2 = dice[2:n])
# ó��???? ?????? ???? ???ұ??? x1, ?ι?°???? ?????? ???ұ??? x2
# dice_df ?? Ȯ??
o <- table(dice_df)
o # 2???? ????ǥ 

# (n-1)/26 = 9.xx

# H0 = ?ΰ??? Ư??�� ???? ?????̴?

rowsum <- apply(o, 1, sum)
colsum <- apply(o, 2, sum)
r <- length(rowsum)
c <- length(colsum)

e <- (rowsum %*% t(colsum)) /sum(o)
d <- (o - e)^2 /e
chisq0 <- sum(d)

alpha <- 0.05
chisq_alpha <- qchisq(1 - alpha, (r-1)*(c-1))

pvalue <- 1 - pchisq(chisq0, (r-1)*(c-1))
pvalue

print(c(chisq0, chisq_alpha, pvalue))

# ???????̶??? ??��?? ?ǽɽ?????.
# ?͹?????�� ?Ⱒ?? ?? ?ִ?.
