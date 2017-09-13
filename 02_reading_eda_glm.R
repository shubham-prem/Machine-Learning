# R - Version 3.4.0

setwd("D:\\Kumbh")
# reading the raw data file which is csv format
library(readr)
pilgdata <- read_csv("pilgrims.csv", col_names = T)
# remvoe last the four columns
names(pilgdata)
pilgdata.47cols <- pilgdata[,-c(48:53)]
names(pilgdata.47cols)
table(pilgdata.47cols$Q27, useNA = "ifany")

summary(pilgdata.47cols$Q27, na.rm=T)

pdatadepcleaned <- subset(pilgdata.47cols, Q27!="", drop=T)
table(pdatadepcleaned$Q27, useNA="ifany")
str(pdatadepcleaned$Q27)
str(pdatadepcleaned$Age)

depvar.hsatisfied <- ifelse(pdatadepcleaned$Q27==1, 1, 0)
str(depvar.hsatisfied)

rm(pilgdata.47cols)
rm(pilgdata)

# prepar independents (features) on subset data "pdatadepcleaned" and fit logistic regression
names(pdatadepcleaned)
str(pdatadepcleaned$Occupation)
table(pdatadepcleaned$Education, useNA = "ifany")
str(pdatadepcleaned$Q15)

table(pdatadepcleaned$Occupation, useNA = "ifany")
table(pdatadepcleaned$Salary, useNA = "ifany")
table(pdatadepcleaned$Q8, useNA = "ifany")
table(pdatadepcleaned$Q9, useNA = "ifany")
table(pdatadepcleaned$Q10, useNA = "ifany")
table(pdatadepcleaned$Q11, useNA = "ifany")
table(pdatadepcleaned$Q12, useNA = "ifany")
table(pdatadepcleaned$Q13, useNA = "ifany")
table(pdatadepcleaned$Q14, useNA = "ifany")

table(pdatadepcleaned$Q15, useNA = "ifany")

table(pdatadepcleaned$Q16, useNA = "ifany")
table(pdatadepcleaned$Q17, useNA = "ifany")
table(pdatadepcleaned$Q18, useNA = "ifany")
table(pdatadepcleaned$Q20, useNA = "ifany")

#there no need of column no Q21 as its shows the place name 

table(pdatadepcleaned$Q22, useNA = "ifany")

table(pdatadepcleaned$Q26, useNA = "ifany")
table(pdatadepcleaned$`Q26 a`, useNA = "ifany")
table(pdatadepcleaned$`Q26 b`, useNA = "ifany")
table(pdatadepcleaned$`Q26 c`, useNA = "ifany")

table(pdatadepcleaned$Q28, useNA = "ifany")

table(pdatadepcleaned$`Q28 a`, useNA = "ifany")

table(pdatadepcleaned$Q29, useNA = "ifany")

table(pdatadepcleaned$Q30, useNA = "ifany")

table(pdatadepcleaned$Q31, useNA = "ifany")

table(pdatadepcleaned$Q31_a, useNA = "ifany")

table(pdatadepcleaned$Q31_b, useNA = "ifany")

table(pdatadepcleaned$Q31_c, useNA = "ifany")

table(pdatadepcleaned$Q31_d, useNA = "ifany")

table(pdatadepcleaned$Q31_e, useNA = "ifany")

table(pdatadepcleaned$Q31_f, useNA = "ifany")

table(pdatadepcleaned$Q31_g, useNA = "ifany")

table(pdatadepcleaned$Q31_h, useNA = "ifany")

table(pdatadepcleaned$Q31_i, useNA = "ifany")

table(pdatadepcleaned$Q31_j, useNA = "ifany")

table(pdatadepcleaned$Q31_k, useNA = "ifany")



# impute education, Q8 with Mode

pdatadepcleaned$Education.imputed <- ifelse(is.na(pdatadepcleaned$Education==T), 5, pdatadepcleaned$Education)

pdatadepcleaned$Occupation <- ifelse(is.na(pdatadepcleaned$Occupation==T), 1, pdatadepcleaned$Occupation)

pdatadepcleaned$Salary <- ifelse(is.na(pdatadepcleaned$Salary==T), 1, pdatadepcleaned$Salary)

pdatadepcleaned$Q8 <- ifelse(is.na(pdatadepcleaned$Q8==T), 1, pdatadepcleaned$Q8)

pdatadepcleaned$Q9 <- ifelse(is.na(pdatadepcleaned$Q9==T), 0, pdatadepcleaned$Q9)

pdatadepcleaned$Q11 <- ifelse(is.na(pdatadepcleaned$Q11==T), 1, pdatadepcleaned$Q11)

pdatadepcleaned$Q12 <- ifelse(is.na(pdatadepcleaned$Q12==T), 2, pdatadepcleaned$Q12)

pdatadepcleaned$Q13 <- ifelse(is.na(pdatadepcleaned$Q13==T), 2, pdatadepcleaned$Q13)

pdatadepcleaned$Q14 <- ifelse(is.na(pdatadepcleaned$Q14==T), 2, pdatadepcleaned$Q14)

pdatadepcleaned$Q15 <- ifelse(is.na(pdatadepcleaned$Q15==T), 2, pdatadepcleaned$Q15)

pdatadepcleaned$Q16 <- ifelse(is.na(pdatadepcleaned$Q16==T), 4, pdatadepcleaned$Q16)

pdatadepcleaned$Q17 <- ifelse(is.na(pdatadepcleaned$Q17==T), 4, pdatadepcleaned$Q17)

pdatadepcleaned$Q18 <- ifelse(is.na(pdatadepcleaned$Q18==T), 1, pdatadepcleaned$Q18)


pdatadepcleaned$Q20 <- ifelse(is.na(pdatadepcleaned$Q20==T), 1, pdatadepcleaned$Q20)

pdatadepcleaned$Q22 <- ifelse(is.na(pdatadepcleaned$Q22==T), 1, pdatadepcleaned$Q22)

pdatadepcleaned$Q26 <- ifelse(is.na(pdatadepcleaned$Q26==T), 1, pdatadepcleaned$Q26)

pdatadepcleaned$Q28 <- ifelse(is.na(pdatadepcleaned$Q28==T), 1, pdatadepcleaned$Q28)

#pdatadepcleaned$`Q28 a` <- ifelse(is.na(pdatadepcleaned$`Q28 a`==T), 1, pdatadepcleaned$`Q28 a`)

pdatadepcleaned$Q29 <- ifelse(is.na(pdatadepcleaned$Q29==T), 2, pdatadepcleaned$Q29)

pdatadepcleaned$`Q26 a`<- ifelse(is.na(pdatadepcleaned$`Q26 a`==T), 1, pdatadepcleaned$`Q26 a`)

pdatadepcleaned$`Q26 b`<- ifelse(is.na(pdatadepcleaned$`Q26 b`==T), 1, pdatadepcleaned$`Q26 b`)

pdatadepcleaned$`Q26 c`<- ifelse(is.na(pdatadepcleaned$`Q26 c`==T), 1, pdatadepcleaned$`Q26 c`)

pdatadepcleaned$Q30 <- ifelse(is.na(pdatadepcleaned$Q30==T), 2, pdatadepcleaned$Q30)

pdatadepcleaned$Q31 <- ifelse(is.na(pdatadepcleaned$Q31==T), 1, pdatadepcleaned$Q31)

summary(glm(depvar.hsatisfied~Age+Education.imputed+Occupation+Salary+Q8+Q9+Q11+Q12+Q13+Q14+Q15+Q16+Q17+Q18+Q20+Q22+Q26+Q28+Q29+`Q26 a`+`Q26 b`+`Q26 c`+`Q28 a`+Q29+Q30+Q31, data=pdatadepcleaned, family="binomial"))
#summary(glm(depvar.hsatisfied~Age+Education.imputed+Occupation+Salary+Q8+Q9+Q11+Q12+Q13+Q14+Q15+Q16+Q17+Q18+Q20+Q22+Q26+Q28+Q29, data=pdatadepcleaned, family="binomial"))

summary(glm(depvar.hsatisfied~Age+Q8+Q9+Q12+Q20+Q26+Q28+Q29+Q31_h+Q31_j, data=pdatadepcleaned, family="binomial"))

summary(glm(depvar.hsatisfied~Age+Q12+Q20+Q26+Q28+Q29+Q31_h+Q31_j, data=pdatadepcleaned, family="binomial"))

#table(pdatadepcleaned$Occupation, useNA="ifany")

names(pdatadepcleaned)
cor(pdatadepcleaned[,c("Q27", "Q29")])

table(pdatadepcleaned$Q29)
q29_pooja <- ifelse(pdatadepcleaned$Q29==1, 1, 0)
table(q29_pooja)
q29_darshan <- ifelse(pdatadepcleaned$Q29==2, 1, 0)
q29_bhajan <- ifelse(pdatadepcleaned$Q29==3, 1, 0)
q26_saftey <- ifelse(pdatadepcleaned$Q26 %in% c(1, 2), 1, 0)
q28_waiting_time_agree <- ifelse(pdatadepcleaned$Q28==1, 1, 0)
# q28_waiting_but_happy <- ifelse(pdatadepcleaned$`Q28 a`==1, 1, 0)

model10 <- glm(depvar.hsatisfied~Age-Q12+Q20+q26_saftey+q28_waiting_time_agree-q28_waiting_but_happy+q29_darshan+Q31_h+Q31_j, data=pdatadepcleaned, family="binomial")
summary(model10)

library(car)
vif(model10)

library(ResourceSelection)
hoslem.test(model10$y, model10$fitted, g=10)

