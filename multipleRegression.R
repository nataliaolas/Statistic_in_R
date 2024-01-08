install.packages("DAAG")
library(DAAG)

df <- ais
str(ais)

#dependent value: percent body fat
#independent value: height, weight, sex

library(tidyverse)
df <- df %>% select(pcBfat, ht, wt, sex)
str(df)

summary(df)

plot(df$sex)

hist(df$pcBfat)
hist(df$ht)
hist(df$wt)

library(psych)

pairs.panels(df[-4], lm = T, stars = T, ci = T, hist.col = "coral4")


model <- lm(pcBfat ~ wt, data = df)
summary(model)

ggplot(df, aes(wt, pcBfat, color = sex)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

model <- lm(pcBfat ~ wt + sex, data = df)
summary(model)

model <- lm(pcBfat ~ ht + wt + sex, data = df)
summary(model)

install.packages("lm.beta")
library("lm.beta")

df$residuals <- residuals(model)
sd(df$residuals)
df$predict <- predict(model, newdata = df)


