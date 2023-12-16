#Test T-Student

help(t.test)

# The assignment of the built-in "iris" dataset in the R language to the variable df, checking columns names, displaying the first few rows of the dataset 

df <- iris
colnames(df)
head(df)

library(ggplot2)


# Creating histogram of Sepal Length

ggplot(df, aes(x = Sepal.Length, fill = Species)) +
  geom_histogram(binwidth = 0.2, alpha = 0.4, position = "identity") +
  geom_vline(aes(xintercept = mean(Sepal.Length)), color = "black", linetype = 2, size = 0.8) +
  geom_rug(alpha = 0.4) +
  scale_fill_manual(values = c("pink", "violet", "orange")) +
  labs(title = "Histogram of Sepal Length", x = "Sepal Length", y = "Frequency")


# Creating vectors from species of iris

seto <- df$Sepal.Length[df$Species == "setosa"]
versi <- df$Sepal.Length[df$Species == "versicolor"]
virg <- df$Sepal.Length[df$Species == "virginica"]

# Combining vectors, creating a dataframe

df <- as.data.frame(cbind(seto,versi,virg))

# First method to check normal distribution - for setosa

qqnorm(seto, pch = 1, frame = T)
qqline(seto, col = "green", lwd = 2)

# Second method to check normal distribution - Shapiro Wilk Test - for setosa

shapiro.test(seto)

# Checking for versicolor

qqnorm(versi, pch = 1, frame = T)
qqline(versi, col = "green", lwd = 2)
shapiro.test(versi)

# Checking for virginica
qqnorm(virg, pch = 1, frame = T)
qqline(virg, col = "green", lwd = 2)
shapiro.test(virg)


#Conclusion: The assumption of normal distribution is met for all three groups


# Checking the equality of variances

var.test(virg, seto) #heterogeneous variance
var.test(versi, seto) #heterogeneous variance
var.test(virg, versi) #homogeneous variance

#H0: There is no difference between virginica and versicolor.
#H1: There is difference between virginica and versicolor. Two means are different.

#dependent values
t.test(df$virg, df$versi, alternative = "two.sided",
       var.equal = T, paired = F)

#Conclusion: Statistically significant difference - we reject the null hypothesis

#H0: There is no difference between virginica and setosa
#H1: There is difference between virginica and setosa. Two means are different.

#indepent values - heteroscedasticity virginica and setosa
t.test(df$virg, df$seto, alternative = "two.sided",
       var.equal = F, paired = F)


#Conclusion: Statistically significant difference - we reject the null hypothesis


#H0: There is no difference between versicolor and setosa
#H1: There is difference between versicolor and setosa. Two means are different.

#dependent values
t.test(df$versi ,df$seto, paired = T)

#Conclusion: Statistically significant difference - we reject the null hypothesis




