x <- c(1,1,2,3.3,3.3,4,4,4,4.7,5,5.6,5.6,5.6,6,6,6.5,6.9)
y <- c(2.3,1.8,2.8,1.8,3.7,2.6,2.6,2.2,3.2,2,3.5,2.8,2.1,3.4,3.2,3.4,5)

data <- as.data.frame(cbind(x, y))

mod1 <- lm(y~x, data=data)
mod2 <- lm(y~0+factor(x), data=data)

#(a)
anova(mod1, mod2)

#(b)
mod3 <- lm(y~poly(x, 3, raw = TRUE), data = data)
mod4 <- lm(y~poly(x, 4, raw = TRUE), data = data)

anova(mod3, mod2)
anova(mod4, mod2)
