# R code for the STT 864 final project
# Flint water problem 
# Peide Li
# Due April 24

# You can find the data set my pdf report at my Github website:
# https://github.com/Kira233767/STT864_Project.git



######################################################################
#Dataset : 
#           Sub_data : Sentinel data set
#           Test_result : Residential testing result
# options(java.parameters = "-Xmx8g")
# 
# library(rJava)    # Assume that the computer has already have Java_Home environment
# library(xlsx)

#Save the data set to be csv files first, then import the data.
Ori.Sub_data <- read.csv("Sentinel_Data_Set_1A-B_515890_7.csv", header = T)
Ori.Sen_data_round2 <- read.csv("Sentinel_Data_Set_1A-B_Rnd_2_517916_7.csv", header = T)
Ori.Sen_data_round3 <- read.csv("Sentinel_Data_Round_3_521415_7.csv", header = T)
Ori.Sen_data_round4 <- read.csv("Sentinel_Data_Round_4_521993_7.csv", header = T)
Test_result <- read.csv("Test_Results_Flint_513922_7.csv", header = T)


temp.dataset <- rbind(Ori.Sub_data, Ori.Sen_data_round2, Ori.Sen_data_round3)


require(nlme)
require(lme4)

#########################################################################################################################################
#Change to Binay Data
flag <- which(temp.dataset$Result_Lead_.PPB. < 15)

Sub_dataset.lesslead <- temp.dataset[flag, ]
Sub_dataset.overlead <- temp.dataset[-flag, ]



Sub_dataset.overlead$Result_Lead_.PPB. <- rep(1, nrow(Sub_dataset.overlead))
Sub_dataset.lesslead$Result_Lead_.PPB. <- rep(0, nrow(Sub_dataset.lesslead))

Sub_data <- rbind(Sub_dataset.overlead, Sub_dataset.lesslead)


# Refine the data set
Index_material <- c(which(Sub_data$Service_Line_Material == "Copper"), 
                    which(Sub_data$Service_Line_Material == "Galvanized"),
                    which(Sub_data$Service_Line_Material == "Lead"))

Sub_data1 <- Sub_data[Index_material, ]

Index_zip.code <- c(which(Sub_data1$Zip_Code == '48503'), which(Sub_data1$Zip_Code == '48504'), 
                    which(Sub_data1$Zip_Code == '48505'), which(Sub_data1$Zip_Code == '48506'), 
                    which(Sub_data1$Zip_Code == '48507'))

Sub_data2 <- Sub_data1[Index_zip.code, ]


Index_sample.location <- c(which(Sub_data2$Sample_Location == 'BATHROOM'), which(Sub_data2$Sample_Location == 'KITCHEN'),
                           which(Sub_data2$Sample_Location == "BASEMENT"))


Sub_data_lead <- Sub_data2[Index_sample.location, ]

Sub_data_lead$Sentinel_Group_Number <- as.factor(Sub_data_lead$Sentinel_Group_Number)
Sub_data_lead$Zip_Code <- as.factor(Sub_data_lead$Zip_Code)




###############################################################
#Explore the distribution of the data set
dis.lead <- table(temp.dataset$Result_Lead_.PPB.)
plot(dis.lead, xlab = "Result_Lead_PPB", ylab = "Number of Observation")

data_at_lead <- Sub_data_lead[which(Sub_data_lead$Service_Line_Material == "Lead"), ]
data_at_copper <- Sub_data_lead[which(Sub_data_lead$Service_Line_Material == "Copper"), ]

plot(table(data_at_copper$Result_Lead_.PPB.), xlab = "Result_Lead_PPB", ylab = "Number of Observation", main = "Line Material Copper")
plot(table(data_at_lead$Result_Lead_.PPB.), xlab = "Result_Lead_PPB", ylab = "Number of Observation", main = "Line Material Lead")


##################################################################################################################################
#Model
######

water.model.lead <- glmer(Result_Lead_.PPB. ~ Service_Line_Material + Zip_Code + (1 | Sample_Location),
                          data = Sub_data_lead, family = binomial)

water.model.lead_cross <- glmer(Result_Lead_.PPB. ~ Service_Line_Material + Zip_Code + Zip_Code * Service_Line_Material + (1 | Sample_Location),
                          data = Sub_data_lead, family = binomial)

plot(fitted(water.model.lead_cross), residuals(water.model.lead_cross, type = "pearson"), xlab = "Predicted value",
     ylab = "Pearson Residuals")


#Likelihood ratio test
water.model.lead_NULL <- glmer(Result_Lead_.PPB. ~ 1 + (1 | Sample_Location),data = Sub_data_lead, family = binomial)


water.model.lead_withoutzip <- glmer(Result_Lead_.PPB. ~ Service_Line_Material  + (1 | Sample_Location),
                                                         data = Sub_data_lead, family = binomial)



anova(water.model.lead_cross, water.model.lead_NULL)

# This proved that logistic regression is valid for the data set 
# Provide more evidence about the logistic regression is valid here



#Cross-validation about prob
flag_cv <- which(Ori.Sen_data_round4$Result_Lead_.PPB. < 15)

cv.lesslead <- Ori.Sen_data_round4[flag_cv, ]
cv.overlead <- Ori.Sen_data_round4[-flag_cv, ]



cv.lesslead$Result_Lead_.PPB. <- rep(1, nrow(cv.lesslead))
cv.overlead$Result_Lead_.PPB. <- rep(0, nrow(cv.overlead))

cv_data <- rbind(cv.overlead, cv.overlead)



Index_material <- c(which(cv_data$Service_Line_Material == "Copper"), 
                    which(cv_data$Service_Line_Material == "Galvanized"),
                    which(cv_data$Service_Line_Material == "Lead"))

cv_data1 <- cv_data[Index_material, ]

Index_zip.code <- c(which(cv_data1$Zip_Code == '48503'), which(cv_data1$Zip_Code == '48504'), 
                    which(cv_data1$Zip_Code == '48505'), which(cv_data1$Zip_Code == '48506'), 
                    which(cv_data1$Zip_Code == '48507'))

cv_data2 <- cv_data1[Index_zip.code, ]


Index_sample.location <- c(which(cv_data2$Sample_Location == 'BATHROOM'), which(cv_data2$Sample_Location == 'KITCHEN'),
                           which(cv_data2$Sample_Location == "BASEMENT"))


cv_data  <- cv_data2[Index_sample.location, ]

cv_data$Sentinel_Group_Number <- as.factor(cv_data$Sentinel_Group_Number)
cv_data$Zip_Code <- as.factor(cv_data$Zip_Code)

target_p <- predict(water.model.lead_cross, newdata = cv_data, type = "response")

cv_pool <- fitted(water.model.lead_cross)
CV_P <- NULL
for (i in 1 : 100){
  temp <- sample(cv_pool, 1000, T)
  CV_P <- c(CV_P, temp)
}
critical <- quantile(sort(CV_P), 0.95)
response_cv <- as.numeric(target_p > critical)
accuracy <- sum(cv_data$Result_Lead_.PPB. == response_cv)/ nrow(cv_data) 



####################################################################################################################
# Performance evaluation
# Need package ROCR
predicted <- predict(water.model.lead_cross)
library(ROCR)
prob <- prediction(predicted, Sub_data_lead$Result_Lead_.PPB.)
tprfpr <- performance(prob, "tpr", "fpr")

plot(tprfpr)

plot(residuals(water.model.lead_cross, type = "deviance"), residuals(water.model.lead_cross, type = "pearson"), 
     xlab = "Deviance", ylab = "Pearson Residuals")








