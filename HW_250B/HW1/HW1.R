# Q2

data1 <- matrix(c(1,15,2,37,3,52,4,59,5,83,6,92), ncol = 2, byrow = T)
colnames(data1) <- c('x', 'y')
data1 <- as.data.frame(data1)
data2 <- matrix(c(6,3882,7,1266,8,733,9,450,10,410,11,305,12,185,13,112), ncol=2,byrow = T)
colnames(data2) <- c('x', 'y')
data2 <- as.data.frame(data2)

R_square <- function(y, y_hat) {
  R1 <- 1 - sum((y - y_hat) ** 2) / sum((y - mean(y)) ** 2)
  R2 <- sum((y_hat - mean(y)) ** 2) / sum((y - mean(y)) ** 2)
  R3 <- sum((y_hat - mean(y_hat)) ** 2) / sum((y - mean(y)) ** 2)
  e <- y - y_hat
  R4 <- 1 - sum((e - mean(e)) ** 2) / sum((y - mean(y)) ** 2)
  R6 <- cor(y, y_hat) ** 2
  R7 <- 1 - sum(e ** 2) / sum(y ** 2)
  R8 <- sum(y_hat ** 2) / sum(y ** 2)
  
  output <- tibble("R1" = R1,
                   "R2" = R2,
                   "R3" = R3,
                   "R4" = R4,
                   "R6" = R6,
                   "R7" = R7,
                   "R8" = R8)
  output
}

mod1 <- lm(y~x, data = data1)
mod2 <- lm(y~0 + x, data = data1)
mod3 <- lm(log(y)~log(x), data = data1)

R_square(data1$y, mod1$fitted.values)
R_square(data1$y, mod2$fitted.values)
R_square(data1$y, exp(mod3$fitted.values))

# Q4 # formula on page 30

y <- c(1,2,3,5,3,1,5,0,6,3,7,4)
x <- c(1,1,1,1,2,2,3,3,4,4,5,5)
z <- c(12,14,16,16,18,16,12,12,10,12,10,16)

mod_q4 <- lm(y~x + z)
summary(mod_q4) # 0.2539
ry1 <- cor(y, x) ** 2
library(ppcor)
ry2.1 <- pcor(t(rbind(y,x,z)))$estimate[1,3]

ry1 + ry2.1 ** 2 * (1 - ry1) # 0.2538
