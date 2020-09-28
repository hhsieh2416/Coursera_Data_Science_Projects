#“Is there an association between college major category and income?”
install.packages("devtools")
devtools::install_github("jhudsl/collegeIncome")
library(collegeIncome)
library(tidyr)
library(ggplot2)
library(dplyr)
data(college)

#1.1 Exploratory Data Analysis
head(college)
str(college)
unique(college$major_category)

median_earing_major <- college %>% 
  group_by(major_category) %>%
  summarize(mean_median = mean(median),
            mean_25 = mean(p25th),
            mean_75 = mean(p75th)) %>%
  gather(key = "variable", value = "value" , -major_category)

ggplot(median_earing_major, aes(major_category, value, group = variable, color = variable)) +
  geom_point() + geom_line() + 
  theme(axis.text.x = element_text(size = 6, angle = 90))

#From the plot, p25th , median and p75th for each major category has similar trend
#so I take median as my outcome in regression model
table(college$major_category)
#There is only one major falls into the "Interdisciplinary" category. 
#it's better to remove it
college <- college %>%
  filter(major_category != "Interdisciplinary")
table(college$major_category)

#2.1 Linear regression
test_1 <- college %>% 
  select(-c("rank","major_code","major","total","sample_size","p25th","p75th"))
str(test_1)
earing_median <- ggplot(test_1, aes(major_category, median, color = major_category))
earing_median + geom_point(size = 2) + theme(axis.text.x = element_text(size = 6, angle = 90))

#everything is comparison of  major_category Agriculture & Natural Resources 
lmfit_1 <- lm(median ~ major_category, test_1) #P>0.05 not significant
#everything is comparison of 0
lmfit_2 <- lm(median ~ major_category -1 , test_1)
#everything is comparison of major_category Business (the highest income)
test_1 <- within(test_1, major_category <- as.factor(major_category))
is.factor(test_1$major_category)
test_2 <- relevel(test_1$major_category, "Business")
lmfit_3 <- lm(median ~ test_2, data = test_1) #p> 0.05 not significant
### P>0.05, which means no significant difference between major category and income
### due to only 173 observations, I think

#check regression assumptions are met with diagnostic plotting (lmfit_1)
resid <- residuals(lmfit_1)
fitted <- fitted.values(lmfit_1)
plot(fitted, resid, xlab = "Predicted values" , ylab = "Residuals")
abline(h = 0, col = "red", lty = "dashed")
#from the plot, normality assumptions don't seem far off, and heteroskedasticity 
#doesn't seem to be an issue.

#overall, there doesn't seem to be an effect of college major category on median income in this study



