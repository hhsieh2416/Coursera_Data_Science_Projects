#“Is there an association between college major category and income?”
install.packages("devtools")
devtools::install_github("jhudsl/collegeIncome")
library(collegeIncome)
library(tidyr)
library(ggplot2)
library(dplyr)
data(college)
devtools::install_github("jhudsl/matahari")
library(matahari)
dance_start(value = FALSE, contents = FALSE)


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

test_1 <- college %>% 
  select(-c("rank","major_code","major","total","sample_size","p25th","p75th"))
str(test_1)
earing_median <- ggplot(test_1, aes(major_category, median, color = major_category))
earing_median + geom_point(size = 2) + theme(axis.text.x = element_text(size = 6, angle = 90))

#everything is comparison of  major_category Agriculture & Natural Resources 
summary(lm(median ~ major_category, test_1))$coef #P>0.05 not significant
#everything is comparison of 0
summary(lm(median ~ major_category -1 , test_1))$coef
#everything is comparison of major_category Interdisciplinary
test_1 <- within(test_1, major_category <- as.factor(major_category))
is.factor(test_1$major_category)
test_2 <- relevel(test_1$major_category, "Interdisciplinary")
summary(lm(median ~ test_2, data = test_1))$coef #p> 0.05 not significant
### P>0.05, which means no significant difference between major category
### due to only 173 observations, I think

dance_save("~/Desktop/college_major_analysis.rds")
