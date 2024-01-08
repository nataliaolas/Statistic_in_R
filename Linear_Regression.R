df <- iris

x <- df$Petal.Width #independent value
y <- df$Petal.Length #depend value

plot(y ~ x)

library(ggplot2)
library(dplyr)

ggplot(df, aes(x, y)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F)

#apply the linear model function
model <- lm(y ~ x)
print(summary(model))


y = 1.08356 + 2.22994 * x

cor(y,x)

a <- data.frame(x = 170)
result <-  predict(model,a)
print(result)

plot(y, x, col = "blue", xlab = "Petal.Width", ylab = "Petal.Length")
abline(lm(x ~ y), col = "red")
