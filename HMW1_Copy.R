getwd()
insurance <- read.csv('file://C:/Users/peter/OneDrive/Documents/NU Graduate/ANA625/insurance.csv')

# Quantitative Techniques for "age" variable:
summary(insurance$age)

# Structure of insurance dataset
str(insurance)

# Central tendencies: Mean, Mode, Median for "age" variable

# function for computing Mode 
Mode = function(x){ 
  ta = table(x)
  tam = max(ta)
  if (all(ta == tam))
    mod = NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(ta)[ta == tam])
  else
    mod = names(ta)[ta == tam]
  return(mod)}

mean(insurance$age)
median(insurance$age)
Mode(insurance$age)

# Max, Min, Interquartile Range, Variance & Standard deviation for "age" variable
max(insurance$age)
min(insurance$age)
IQR(insurance$age)
var(insurance$age)
sd(insurance$age)

# Skewness and Kurtosis for "age" variable
library(moments)
skewness(insurance$age)

# Graphical Techniques with insurance dataset:

# Scatter plot for two variables: "age" vs. "bmi"
plot(insurance$age,insurance$bmi, main="Scatter plot: Age vs. BMI", xlab="Age (years)",
     ylab="BMI (cm/kg)", col = "goldenrod1", pch=19)
abline(lm(insurance$bmi ~ insurance$age), col = "black")

cor(insurance$age,insurance$bmi)

# Box plot of "age" variable
boxplot(insurance$age, main="Boxplot of Age",
        ylab="Age in years", col = "pink")


# Histogram for "age" variable
hist(insurance$age, col="cadetblue", main = "Histogram of Age", breaks = 20)

# Kernel Density Estimation of "age" variable
d <- density(insurance$age)
plot(d, main= "KDE of Age")
polygon(d, col = "lightpink", border = "black")

# Continuous variables in insurance dataset
cont_ins <- insurance[c("age", "bmi", "children", "charges")]

# Scatter plot matrix of continuous variables in insurance dataset
library(psych)
pairs.panels(cont_ins)


# Q-Q plot for "age" variable
qqnorm(insurance$age, main = "Q-Q Plot for Age")
qqline(insurance$age)