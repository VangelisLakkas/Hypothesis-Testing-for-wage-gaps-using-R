require(foreign)
library(moments)
library(nortest)
library(psych)
library(Hmisc)
library(car)
library(sjPlot)
library(ggplot2)
library(gmodels)

Salary_DF <- read.spss(file = "salary.sav", to.data.frame = TRUE)
str(Salary_DF)
summary(Salary_DF)
tail(Salary_DF)
length(Salary_DF[,1])  #sample size

index <- sapply(Salary_DF, class) == "numeric"
DF_num_variables <- Salary_DF[index]
head(DF_num_variables)
DF_num_variables<-DF_num_variables[,-1]

sapply(DF_num_variables, summary)
par(mfrow =c(2,3))
hist(sal_num$salbeg,main = 'Beginning Salary',col = 'lightblue')
hist(sal_num$time, main = 'Time', col = 'lightblue')
hist(sal_num$age, main = 'Age', col='lightblue')
hist(sal_num$salnow, main = 'Current Salary', col = 'lightblue')
hist(sal_num$edlevel, main='Education Level', col= 'lightblue')
hist(sal_num$work, main = 'Work', col = 'lightblue')

sapply(DF_num_variables, skewness)
sapply(DF_num_variables, kurtosis)

sapply(DF_num_variables, lillie.test)
sapply(DF_num_variables,shapiro.test)

v <- ncol(DF_num_variables)
par(mfrow = c(1,1))
for (i in 1:v){
  qqnorm(DF_num_variables[,i])
  qqline(DF_num_variables[,i])
}

##Task 3 
Begin_Sal<-DF_num_variables$salbeg
qqnorm(Begin_Sal, main = "QQplot for Beginning Salary"); qqline(Begin_Sal)#reject normality
lillie.test(Begin_Sal)#reject normality
shapiro.test(Begin_Sal)#reject normality
length(Begin_Sal) # Big sample size, length>50
summary(Begin_Sal) ## median has big difference from the mean,mean not sufficient for central location
skewness(Begin_Sal) ; kurtosis(Begin_Sal) ##mean not sufficient descriptive measure for central location, skewness and kurtosis differs significantly from those of normal distribution
wilcox.test(Begin_Sal,mu = 1000, conf.level = 0.95)#reject null hypothesis

##Task 4
logdiff <- log(DF_num_variables$salnow-Begin_Sal)
qqnorm(logdiff, main="QQplot for logdiff") ; qqline(logdiff)#reject normality
length(logdiff) ## n>50
lillie.test(logdiff)#reject normality
shapiro.test(logdiff)#reject normality
summary(logdiff)#mean sufficient for central location
skewness(logdiff) ; kurtosis(logdiff) #mean sufficient for central location, skewness and kurtosis differ reasonably from those of normal distribution
t.test(logdiff, mu=1, conf.level = 0.95)#reject null hypothesis

#Task 5
#Independent samples
men_group <- Salary_DF$salbeg[Salary_DF$sex=='MALES']
length(men_group) ##n1=258 > 50, big sample
women_group <- Salary_DF$salbeg[Salary_DF$sex=='FEMALES']
length(women_group)
temp_dataset1 <- subset.data.frame(Salary_DF, select = c('salbeg', 'sex'))

by(temp_dataset1$salbeg,temp_dataset1$sex, lillie.test) ##reject normality
by(temp_dataset1$salbeg,temp_dataset1$sex, shapiro.test) ##reject normality
par(mfrow=c(1,1))
qqnorm(men_group, main = "QQplot for men's beginning salary") ; qqline(men_group) ##reject normality, QQplot not alligned with qqline
qqnorm(women_group,  main = "QQplot for women's beginning salary") ; qqline(women_group) ##reject normality, QQplot not alligned with qqline

by(temp_dataset1$salbeg, temp_dataset1$sex, summary) ##mean not sufficient descriptive measure for central location
skewness(men_group) ; kurtosis(men_group) ##mean not sufficient descriptive measure for central location, skewness and kurtosis differs significantly from those of normal distribution
skewness(women_group) ; kurtosis(women_group)##mean not sufficient descriptive measure for central location, skewness and kurtosis differs significantly from those of normal distribution

wilcox.test(temp_dataset1$salbeg~temp_dataset1$sex)##Reject null hypothesis, there is difference for the beginning salary
##between men and women
boxplot(men_group,women_group, main='SalBeg per Gender Boxplot', ylab = 'Beginning Salary' ,xlab='MALES FEMALES', col = c('blue', 'purple'))

##Task 6
age_cut <- cut2(DF_num_variables$age, g=3)
Salary_DF$age_cut <- age_cut
relSal <- ((DF_num_variables$salnow - DF_num_variables$salbeg)/DF_num_variables$salnow) * (1/DF_num_variables$time)
relSal
Salary_DF$relSal <- relSal
age_table <- table(age_cut)
age_table ##n1, n2 and n3 > 50 big sample
head(Salary_DF)

anv1 <- aov(Salary_DF$relSal~age_cut)
anv1
summary(anv1)
anv1$residuals
lillie.test(anv1$residuals) ##do not reject normality
shapiro.test(anv1$residuals) ##reject normality
qqnorm(anv1$residuals, main = 'QQplot for the residuals');qqline(anv1$residuals) ##the tails decline from normal distribution


age_group1_sal <- Salary_DF$relSal[Salary_DF$age_cut=='[23.0,29.7)']
age_group2_sal <- Salary_DF$relSal[Salary_DF$age_cut=='[29.7,39.8)']
age_group3_sal <- Salary_DF$relSal[Salary_DF$age_cut=='[39.8,64.5]']

mean(age_group1_sal) ; median(age_group1_sal) ##mean sufficient descriptive measure for central location
mean(age_group2_sal) ; median(age_group2_sal) ##mean sufficient descriptive measure for central location
mean(age_group3_sal) ; median(age_group3_sal) ##mean sufficient descriptive measure for central location

leveneTest(relSal~age_cut, data = Salary_DF, center=mean)
## Homogeneity not rejected, I used levene test and not barlett because my data are not
##normally distributed and levene barlett is more robust for not normally distributed data

summary(anv1) ## p-value too small reject H0
pairwise.t.test(Salary_DF$relSal, Salary_DF$age_cut, p.adjust.method = p.adjust.methods[1])##There are significant differences

mean_relSal <- tapply(Salary_DF$relSal, Salary_DF$age_cut, mean)
sd_relSal <- tapply(Salary_DF$relSal, Salary_DF$age_cut, sd)
age_groups <- unique(Salary_DF$age_cut)
errorbar_data <- data.frame(age_groups, mean_relSal, sd_relSal)

ggplot(errorbar_data, aes(x = age_groups, y = mean_relSal)) +
  geom_errorbar(aes(ymin = mean_relSal - sd_relSal, ymax = mean_relSal + sd_relSal, col='red'),
                width = 0.25, position = position_dodge(0.5)) +
  labs(x = "Age Groups", y = "Mean relSal", title = "Error Bar for Relative Salary Rise per age group") +
  theme_minimal()


##Task 7
levels(Salary_DF$jobcat)
table(Salary_DF$jobcat)
table(factor(Salary_DF$jobcat)) ##we do not have big sample for all the variables, n3-n7 < 50 

job_cat_anova <- aov(Salary_DF$relSal~factor(Salary_DF$jobcat))
summary(job_cat_anova)
shapiro.test(job_cat_anova$residuals)  ##H0 reject
lillie.test(job_cat_anova$residuals)##H0 reject
qqnorm(job_cat_anova$residuals);qqline(job_cat_anova$residuals) ##the tails decline from normal distribution especially on the right
kruskal.test(Salary_DF$relSal~factor(Salary_DF$jobcat))  ##H0 reject
pairwise.wilcox.test(Salary_DF$relSal, Salary_DF$jobcat)
boxplot(Salary_DF$relSal~Salary_DF$jobcat, col='GREEN', main='RelSal for each job category Boxplot', xlab = 'Job Category', ylab = 'Relative Salary Rise')


##Task 8
age_cut2 <- cut(DF_num_variables$age, quantile(Salary_DF$age, probs = c(0, 0.25, 0.5, 0.75, 1)))
Salary_DF$age_cut2 <- age_cut2
levels(age_cut2)
table(age_cut2) ##n1,n2,n3,n4>50 big samples
is.factor(age_cut2)
anova_Task8 <- aov(Salary_DF$salbeg~age_cut2)
summary(anova_Task8)
lillie.test(anova_Task8$residuals) ##reject null hypothesis for normality
shapiro.test(anova_Task8$residuals) ##reject null hypothesis for normality
qqnorm(anova_Task8$residuals);qqline(anova_Task8$residuals) ##the tails decline from normal distribution especially on the right
head(Salary_DF)

age_group1_salbeg <- Salary_DF$salbeg[Salary_DF$age_cut2=="(23,28.5]"&!is.na(Salary_DF$age_cut2)]
age_group2_salbeg <- Salary_DF$salbeg[Salary_DF$age_cut2=="(28.5,32]"&!is.na(Salary_DF$age_cut2)]
age_group3_salbeg <- Salary_DF$salbeg[Salary_DF$age_cut2=="(32,46]"&!is.na(Salary_DF$age_cut2)]
age_group4_salbeg <- Salary_DF$salbeg[Salary_DF$age_cut2=="(46,64.5]"&!is.na(Salary_DF$age_cut2)]


mean(age_group1_salbeg) ; median(age_group1_salbeg)   ##mean is not sufficient descriptive measure for central location
mean(age_group2_salbeg) ; median(age_group2_salbeg) ##mean is not sufficient descriptive measure for central location
mean(age_group3_salbeg) ; median(age_group3_salbeg) ##mean is not sufficient descriptive measure for central location
mean(age_group4_salbeg) ; median(age_group4_salbeg) ##mean is not sufficient descriptive measure for central location

skewness(age_group1_salbeg) ; kurtosis(age_group1_salbeg)
skewness(age_group2_salbeg) ; kurtosis(age_group2_salbeg)
skewness(age_group3_salbeg) ; kurtosis(age_group3_salbeg)
skewness(age_group4_salbeg) ; kurtosis(age_group4_salbeg)
kruskal.test(Salary_DF$salbeg~Salary_DF$age_cut2)  ##H0 reject, beginning salary is not the same on average for all age groups
pairwise.wilcox.test(Salary_DF$salbeg,Salary_DF$age_cut2)
boxplot(Salary_DF$salbeg~Salary_DF$age_cut2, col='orange', main='Salbeg per age group Boxplot', xlab = 'Age Group', ylab = 'Beginning Salary')

##task9
minority_table <- table(Salary_DF$sex, Salary_DF$minority)
minority_table
mean(chisq.test(Salary_DF$sex, Salary_DF$minority)$expected>5) ##expected  values>5
prop_minority_table <- prop.table(minority_table)
prop_minority_table
CrossTable(Salary_DF$sex, Salary_DF$minority)
prop.test(minority_table)
chisq.test(minority_table) ##do not reject H0

##task 10
JobMinority_table <- table(Salary_DF$jobcat, Salary_DF$minority)
JobMinority_table
prop_tab1 <-prop.table(JobMinority_table)
prop_tab1
mean(chisq.test(JobMinority_table)$expected>5) ## small expected values
chisq.test(JobMinority_table, simulate.p.value = T) ##reject H0
fisher.test(JobMinority_table, simulate.p.value = T) ##reject H0






