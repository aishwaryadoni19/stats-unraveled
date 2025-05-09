# Module 4 R PRACTICE

install.packages("MASS")
library(MASS)

data(cats)
head(cats)
str(cats)
summary(cats)

# PART 1

# Subset the data by gender
male <- subset(cats, subset = (cats$Sex == "M"))$Bwt
female <- subset(cats, subset = (cats$Sex == "F"))$Bwt

# Perform a two-sample t-test with unequal variance
t_test_result <- t.test(male, female, var.equal = FALSE)
print(t_test_result)

# PART 2

# Sleeping quality scores
before <- c(4.6, 7.8, 9.1, 5.6, 6.9, 8.5, 5.3, 7.1, 3.2, 4.4)
after <- c(6.6, 7.7, 9.0, 6.2, 7.8, 8.3, 5.9, 6.5, 5.8, 4.9)

# Perform a paired t-test
paired_t_test <- t.test(after, before, paired = TRUE, alternative = "greater")
print(paired_t_test)
