##Trees and Decision Forests
library(survey)
library(foreign)
library(knitr)
library(rpart)
library(rpart.plot)
library(randomForest)
library(xtable)
#install.packages("survey")
#-------------------------------------------------------------------------------------------------------
#Explanatory Models
techds <- read.spss("ATP W99.sav", to.data.frame = TRUE)
View(techds)

install.packages("dplyr") # Install dplyr, if you haven't already
library(dplyr) # Load the dplyr package
all_dummy <- techds %>%
  mutate(SC1_W99_dummy = case_when(
    SC1_W99 == "Refused" ~ 0,
    SC1_W99 == "Mostly negative" ~ 1,
    SC1_W99 == "Equal positive and negative effects" ~ 2,
    SC1_W99 == "Mostly positive" ~ 3,
    TRUE ~ NA_real_ # This line handles cases that don't match any of the above conditions
  ))

all_dummy <- all_dummy %>%
  mutate(BCHIP14_a_W99_dummy = case_when(
    BCHIP14_a_W99 == "Refused" ~ 0,
    BCHIP14_a_W99 == "Oppose" ~ 1,
    BCHIP14_a_W99 == "Not sure" ~ 2,
    BCHIP14_a_W99 == "Favor" ~ 3,
    TRUE ~ NA_real_
  ))


all_dummy <- all_dummy %>%
  mutate(BCHIP14_b_W99_dummy = case_when(
    BCHIP14_b_W99 == "Refused" ~ 0,
    BCHIP14_b_W99 == "Oppose" ~ 1,
    BCHIP14_b_W99 == "Not sure" ~ 2,
    BCHIP14_b_W99 == "Favor" ~ 3,
    TRUE ~ NA_real_
  ))


all_dummy <- all_dummy %>%
  mutate(BCHIP14_c_W99_dummy = case_when(
    BCHIP14_c_W99 == "Refused" ~ 0,
    BCHIP14_c_W99 == "Oppose" ~ 1,
    BCHIP14_c_W99 == "Not sure" ~ 2,
    BCHIP14_c_W99 == "Favor" ~ 3,
    TRUE ~ NA_real_
  ))


all_dummy <- all_dummy %>%
  mutate(BCHIP14_d_W99_dummy = case_when(
    BCHIP14_d_W99 == "Refused" ~ 0,
    BCHIP14_d_W99 == "Oppose" ~ 1,
    BCHIP14_d_W99 == "Not sure" ~ 2,
    BCHIP14_d_W99 == "Favor" ~ 3,
    TRUE ~ NA_real_
  ))

all_dummy <- all_dummy %>%
  mutate(GENEV4_a_W99_dummy = case_when(
    GENEV4_a_W99 == "Refused" ~ 0,
    GENEV4_a_W99 == "Definitely would NOT happen" ~ 1,
    GENEV4_a_W99 == "Probably would happen" ~ 2,
    GENEV4_a_W99 == "Definitely would happen" ~ 3,
    TRUE ~ NA_real_
  ))

all_dummy <- all_dummy %>%
  mutate(GENEV4_b_W99_dummy = case_when(
    GENEV4_b_W99 == "Refused" ~ 0,
    GENEV4_b_W99 == "Definitely would NOT happen" ~ 1,
    GENEV4_b_W99 == "Probably would happen" ~ 2,
    GENEV4_b_W99 == "Definitely would happen" ~ 3,
    TRUE ~ NA_real_
  ))

all_dummy <- all_dummy %>%
  mutate(GENEV4_c_W99_dummy = case_when(
    GENEV4_c_W99 == "Refused" ~ 0,
    GENEV4_c_W99 == "Definitely would NOT happen" ~ 1,
    GENEV4_c_W99 == "Probably would happen" ~ 2,
    GENEV4_c_W99 == "Definitely would happen" ~ 3,
    TRUE ~ NA_real_
  ))

all_dummy <- all_dummy %>%
  mutate(GENEV4_d_W99_dummy = case_when(
    GENEV4_d_W99 == "Refused" ~ 0,
    GENEV4_d_W99 == "Definitely would NOT happen" ~ 1,
    GENEV4_d_W99 == "Probably would happen" ~ 2,
    GENEV4_d_W99 == "Definitely would happen" ~ 3,
    TRUE ~ NA_real_
  ))

all_dummy <- all_dummy %>%
  mutate(GENEV3_W99_dummy = case_when(
    GENEV3_W99 == "Refused" ~ 0,
    GENEV3_W99 == "No, I would definitely NOT want this for my baby" ~ 1,
    GENEV3_W99 == "Yes, I would probably want this for my baby" ~ 2,
    GENEV3_W99 == "Yes, I would definitely want this for my baby" ~ 3,
    TRUE ~ NA_real_
  ))

all_dummy <- all_dummy %>%
  mutate(BCHIP3_W99_dummy = case_when(
    BCHIP3_W99 == "Refused" ~ 1,
    BCHIP3_W99 == "Definitely NOT want" ~ 2,
    BCHIP3_W99 == "Probably NOT want" ~ 3,
    BCHIP3_W99 == "Probably want" ~ 4,
    BCHIP3_W99 == "Definitely want" ~ 5,
    TRUE ~ NA_real_
  ))


expl_model1 <- glm(GENEV4_a_W99_dummy ~ BCHIP14_a_W99_dummy, data=all_dummy)
summary(expl_model1)

expl_model2 <- glm(GENEV4_a_W99_dummy ~ BCHIP14_a_W99_dummy + BCHIP14_b_W99_dummy , data=all_dummy)
summary(expl_model2)

expl_model3 <- glm(GENEV4_a_W99_dummy ~ BCHIP14_a_W99_dummy + BCHIP14_b_W99_dummy + BCHIP14_c_W99_dummy + BCHIP14_d_W99_dummy, data=all_dummy)
summary(expl_model3)

expl_model4 <- glm(GENEV4_b_W99_dummy ~ BCHIP14_a_W99_dummy + BCHIP14_b_W99_dummy + BCHIP14_c_W99_dummy + BCHIP14_d_W99_dummy, data=all_dummy)
summary(expl_model4)

expl_model5 <- glm(GENEV4_c_W99_dummy ~ BCHIP14_a_W99_dummy + BCHIP14_b_W99_dummy + BCHIP14_c_W99_dummy + BCHIP14_d_W99_dummy, data=all_dummy)
summary(expl_model5)

expl_model6 <- glm(GENEV4_d_W99_dummy ~ BCHIP14_a_W99_dummy + BCHIP14_b_W99_dummy + BCHIP14_c_W99_dummy + BCHIP14_d_W99_dummy, data=all_dummy)
summary(expl_model6)

expl_model7 <- glm(GENEV3_W99_dummy ~ BCHIP3_W99_dummy, data=all_dummy)
summary(expl_model7)

expl_model8 <- glm(BCHIP3_W99_dummy ~ SC1_W99_dummy, data=all_dummy)
summary(expl_model8)

expl_model9 <- glm(GENEV3_W99_dummy ~ SC1_W99_dummy, data=all_dummy)
summary(expl_model9)

stargazer(expl_model3, expl_model4, expl_model5, expl_model6, expl_model7)
stargazer(expl_model8, expl_model9)
#-------------------------------------------------------------------------------------------------------
#Predictive Models
techds <- read.spss("ATP W99.sav", to.data.frame = TRUE)

#Create new dataframe with variables of interest
tree_df <- data.frame(BCHIP3 = techds$BCHIP3_W99,
                      BCHIP14_A = techds$BCHIP14_a_W99, 
                      BCHIP14_B = techds$BCHIP14_b_W99, 
                      BCHIP14_C = techds$BCHIP14_c_W99, 
                      BCHIP14_D = techds$BCHIP14_d_W99,
                      GENEV3 = techds$GENEV3_W99,
                      GENEV4_A = techds$GENEV4_a_W99,
                      GENEV4_B = techds$GENEV4_b_W99, 
                      GENEV4_C = techds$GENEV4_c_W99, 
                      GENEV4_D = techds$GENEV4_d_W99)

tree_df_clean <- na.omit(tree_df[, c("BCHIP3", "BCHIP14_A", "BCHIP14_B", "BCHIP14_C", "BCHIP14_D",
                                    "GENEV3", "GENEV4_A","GENEV4_B", "GENEV4_C", "GENEV4_D")])

#Create a training and testing dataset
set.seed(123)

# Splitting the data into train and test sets
train_prop <- 0.75
train_size <- round(train_prop * nrow(tree_df_clean))
train_indices <- sample(1:nrow(tree_df_clean), train_size)
train <- tree_df_clean[train_indices, ]
test <- tree_df_clean[-train_indices, ]

###GENEV3
# Fitting the decision tree model for GENEV3
fit_genev3 <- rpart(GENEV3 ~ . - BCHIP14_A - BCHIP14_B - BCHIP14_C - BCHIP14_D - GENEV4_A - GENEV4_B - GENEV4_C - GENEV4_D , 
                    data = train, method = "class")
printcp(fit_genev3)  
optimal_cpgenev3 <- fit_genev3$cptable[which.min(fit_genev3$cptable[, "xerror"]), "CP"]
tree_genev3_train <- rpart(GENEV3 ~ . - BCHIP14_A - BCHIP14_B - BCHIP14_C - BCHIP14_D - GENEV4_A - GENEV4_B - GENEV4_C - GENEV4_D , 
                           data = train, method = "class", cp = optimal_cpgenev3)

rpart.plot(tree_genev3_train, type = 4, extra = 100, box.palette = "BuGn", legend.y = 0.0005, legend.x = 0.1)

tree_genev3_test <- predict(tree_genev3_train, newdata = test, type = "class")
error_rate_genev3 <- mean(tree_genev3_test != test$GENEV3)
print(error_rate_genev3)

conf_matrix <- table(test$GENEV3, tree_genev3_test)
conf_matrix_table <- xtable(conf_matrix, caption = "Confusion matrix")
print(conf_matrix_table, caption.placement = "top")

set.seed(123)
rf_genev3 <- randomForest(GENEV3 ~ BCHIP3, data = train, ntree = 10)
importance(rf_genev3)
varImpPlot(rf_genev3)
rf_genev3_test <- predict(rf_genev3, test)
conf_matrix_rf_g3 <- table(test$GENEV3, rf_genev3_test)
conf_matrix_rf_table_g3 <- xtable(conf_matrix_rf_g3, caption = "Confusion Matrix Random Forest GENEV3 and BCHIP3")
print(conf_matrix_rf_table_g3, caption.placement = "top")
error_rate_genevg3_rf <- mean(rf_genev3_test != test$GENEV3)
print(error_rate_genevg3_rf)

##GENEV4_A
fit_genev4_a <- rpart(GENEV4_A ~ . - BCHIP3 - GENEV3 - GENEV4_B - GENEV4_C - GENEV4_D, 
                      data = train, method = "class")
printcp(fit_genev4_a)  
optimal_cpgenev4_a <- fit_genev4_a$cptable[which.min(fit_genev4_a$cptable[, "xerror"]), "CP"]
tree_genev4a_train <- rpart(GENEV4_A ~ . - BCHIP3 - GENEV3 - GENEV4_B - GENEV4_C - GENEV4_D, 
                           data = train, method = "class", cp = optimal_cpgenev4_a)

rpart.plot(tree_genev4a_train, type = 4, extra = 100, box.palette = "BuGn", legend.y = 0.0005, legend.x = 0.1)

tree_genev4_a_test <- predict(tree_genev4a_train, newdata = test, type = "class")
error_rate_genev4a <- mean(tree_genev4_a_test != test$GENEV4_A)
print(error_rate_genev4a)

conf_matrix_genev4_a <- table(test$GENEV4_A, tree_genev4_a_test)
conf_matrix_table_a <- xtable(conf_matrix_genev4_a, caption = "Confusion Matrix GENEV4_A and BCHIP14")
print(conf_matrix_table_a, caption.placement = "top")

set.seed(123)
rf_genev4_a <- randomForest(GENEV4_A ~ . - BCHIP3 - GENEV3 - GENEV4_B - GENEV4_C - GENEV4_D, 
                      data = train, ntree = 5000)
rf_genev4_a_test <- predict(rf_genev4_a, test)
importance(rf_genev4_a)
varImpPlot(rf_genev4_a)
conf_matrix_rf_genev4_a <- table(test$GENEV4_A, rf_genev4_a_test)
conf_matrix_rf_table_a <- xtable(conf_matrix_rf_genev4_a, caption = "Confusion Matrix Random Forest GENEV4_A and BCHIP14")
print(conf_matrix_rf_table_a, caption.placement = "top")
error_rate_genev4a_rf <- mean(rf_genev4_a_test != test$GENEV4_A)
print(error_rate_genev4a_rf)

##GENEV4_B
fit_genev4_b <- rpart(GENEV4_B ~ . - BCHIP3 - GENEV3 - GENEV4_A - GENEV4_C - GENEV4_D, 
                      data = train, method = "class")
printcp(fit_genev4_b)  
optimal_cpgenev4_b <- fit_genev4_b$cptable[which.min(fit_genev4_b$cptable[, "xerror"]), "CP"]
tree_genev4b_train <- rpart(GENEV4_B ~ . - BCHIP3 - GENEV3 - GENEV4_A - GENEV4_C - GENEV4_D,
                            data = train, method = "class", cp = optimal_cpgenev4_b)

rpart.plot(tree_genev4b_train, type = 4, extra = 100, box.palette = "BuGn", legend.y = 0.0005, legend.x = 0.1)

tree_genev4_b_test <- predict(tree_genev4b_train, newdata = test, type = "class")
error_rate_genev4b<- mean(tree_genev4_b_test != test$GENEV4_B)
print(error_rate_genev4b)

conf_matrix_genev4_b <- table(test$GENEV4_B, tree_genev4_b_test)
conf_matrix_table_b <- xtable(conf_matrix_genev4_b, caption = "Confusion Matrix GENEV4_B and BCHIP14")
print(conf_matrix_table_b, caption.placement = "top")

set.seed(123)
rf_genev4_b <- randomForest(GENEV4_B ~ . - BCHIP3 - GENEV3 - GENEV4_A - GENEV4_C - GENEV4_D, 
                            data = train, ntree = 5000)
rf_genev4_b_test <- predict(rf_genev4_b, test)
importance(rf_genev4_b)
varImpPlot(rf_genev4_b)
conf_matrix_rf_genev4_b <- table(test$GENEV4_B, rf_genev4_b_test)
conf_matrix_rf_table_b <- xtable(conf_matrix_rf_genev4_b, caption = "Confusion Matrix Random Forest GENEV4_B and BCHIP14")
print(conf_matrix_rf_table_b, caption.placement = "top")
error_rate_genev4b_rf <- mean(rf_genev4_b_test != test$GENEV4_B)
print(error_rate_genev4b_rf)

###GENEV4_C
fit_genev4_c <- rpart(GENEV4_C ~ . - BCHIP3 - GENEV3 - GENEV4_A - GENEV4_B - GENEV4_D, 
                    data = train, method = "class")
printcp(fit_genev4_c)  
optimal_cpgenev4_c <- fit_genev4_c$cptable[which.min(fit_genev4_c$cptable[, "xerror"]), "CP"]
tree_genev4_train <- rpart(GENEV4_C ~ . - BCHIP3 - GENEV3 - GENEV4_A - GENEV4_B - GENEV4_D, 
                           data = train, method = "class", cp = optimal_cpgenev4_c)

rpart.plot(tree_genev4_train, type = 4, extra = 100, box.palette = "BuGn", legend.y = 0.0005, legend.x = 0.1)

tree_genev4_c_test <- predict(tree_genev4_train, newdata = test, type = "class")
error_rate_genev4c<- mean(tree_genev4_c_test != test$GENEV4_C)
print(error_rate_genev4c)

conf_matrix <- table(test$GENEV4_C, tree_genev4_c_test)
conf_matrix_table <- xtable(conf_matrix, caption = "Confusion Matrix GENEV4_C & BCHIP14")
print(conf_matrix_table, caption.placement = "top")

set.seed(123)
rf_genev4_c <- randomForest(GENEV4_C ~ . - BCHIP3 - GENEV3 - GENEV4_A - GENEV4_B - GENEV4_D, 
                            data = train, ntree = 5000)
rf_genev4_c_test <- predict(rf_genev4_c, test)
importance(rf_genev4_c)
varImpPlot(rf_genev4_c)
conf_matrix_rf_genev4_c <- table(test$GENEV4_C, rf_genev4_c_test)
conf_matrix_rf_table_c <- xtable(conf_matrix_rf_genev4_c, caption = "Confusion Matrix Random Forest GENEV4_C & BCHIP14")
print(conf_matrix_rf_table_c, caption.placement = "top")
error_rate_genev4c_rf <- mean(rf_genev4_c_test != test$GENEV4_C)
print(error_rate_genev4c_rf)

##GENEV4_D
fit_genev4_d <- rpart(GENEV4_D ~ . - BCHIP3 - GENEV3 - GENEV4_A - GENEV4_B - GENEV4_C, 
                      data = train, method = "class")
printcp(fit_genev4_d)  
optimal_cpgenev4_d <- fit_genev4_d$cptable[which.min(fit_genev4_d$cptable[, "xerror"]), "CP"]
tree_genev4_d_train <- rpart(GENEV4_D ~ . - BCHIP3 - GENEV3 - GENEV4_A - GENEV4_B - GENEV4_C, 
                           data = train, method = "class", cp = optimal_cpgenev4_d)

rpart.plot(tree_genev4_d_train, type = 4, extra = 100, box.palette = "BuGn", legend.y = 0.0005, legend.x = 0.1)

tree_genev4_d_test <- predict(tree_genev4_d_train, newdata = test, type = "class")
error_rate_genev4d<- mean(tree_genev4_d_test != test$GENEV4_D)
print(error_rate_genev4d)
conf_matrix_genev4_d <- table(test$GENEV4_D, tree_genev4_d_test)
conf_matrix_table_4d <- xtable(conf_matrix_genev4_d, caption = "Confusion Matrix GENEV4_D and BCHIP14")
print(conf_matrix_table_4d, caption.placement = "top")

set.seed(123)
rf_genev4_d <- randomForest(GENEV4_D ~ . - BCHIP3 - GENEV3 - GENEV4_A - GENEV4_B - GENEV4_C, 
                            data = train, ntree = 5000)
rf_genev4_d_test <- predict(rf_genev4_d, test)
importance(rf_genev4_d)
varImpPlot(rf_genev4_d)
conf_matrix_rf_genev4_d <- table(test$GENEV4_D, rf_genev4_d_test)
conf_matrix_rf_table_d <- xtable(conf_matrix_rf_genev4_d, caption = "Confusion Matrix Random Forest GENEV4_D and BCHIP14")
print(conf_matrix_rf_table_d, caption.placement = "top")
error_rate_genev4d_rf <- mean(rf_genev4_d_test != test$GENEV4_D)
print(error_rate_genev4d_rf)

#------------------------------------------------------------------------------------------------------------------------------------------------
tree_df_b3 <- data.frame(SC1 = techds$SC1_W99,
                         BCHIP3 = techds$BCHIP3_W99)

tree_df_b14 <- data.frame(SC1 = techds$SC1_W99,
                      BCHIP14_A = techds$BCHIP14_a_W99, 
                      BCHIP14_B = techds$BCHIP14_b_W99, 
                      BCHIP14_C = techds$BCHIP14_c_W99, 
                      BCHIP14_D = techds$BCHIP14_d_W99)

tree_df_g3 <- data.frame(SC1 = techds$SC1_W99,
                        GENEV3 = techds$GENEV3)

tree_df_g4 <- data.frame(SC1 = techds$SC1_W99,
                        GENEV4_A = techds$GENEV4_a_W99, 
                        GENEV4_B = techds$GENEV4_b_W99, 
                        GENEV4_C = techds$GENEV4_c_W99, 
                        GENEV4_D = techds$GENEV4_d_W99)

tree_df_b3_clean <- na.omit(tree_df_b3[, c("SC1", "BCHIP3")])
train_indices_b3 <- sample(1:nrow(tree_df_b3_clean), train_size)
trainb3 <- tree_df_b3_clean[train_indices_b3, ]
testb3 <- tree_df_b3_clean[-train_indices, ]

tree_df_b14_clean <- na.omit(tree_df_b14[, c("SC1", "BCHIP14_A", "BCHIP14_B", "BCHIP14_C", "BCHIP14_D")])
train_indices_b14 <- sample(1:nrow(tree_df_b14_clean), train_size)
trainb14 <- tree_df_b14_clean[train_indices_b14, ]
testb14 <- tree_df_b14_clean[-train_indices, ]

tree_df_g3_clean<- na.omit(tree_df_g3[, c("SC1", "GENEV3")])
train_indices_g3 <- sample(1:nrow(tree_df_g3_clean), train_size)
traing3 <- tree_df_g3_clean[train_indices_g3, ]
testg3 <- tree_df_g3_clean[-train_indices, ]

tree_df_g4_clean <- na.omit(tree_df_g4[, c("SC1", "GENEV4_A", "GENEV4_B", "GENEV4_C", "GENEV4_D")])
train_indices_g4 <- sample(1:nrow(tree_df_g4_clean), train_size)
traing4 <- tree_df_g4_clean[train_indices_g4, ]
testg4 <- tree_df_g4_clean[-train_indices, ]

#SC1 and BCHIP3
fit.bch3 <- rpart(SC1 ~ . , data = tree_df_b3_clean)
printcp(fit.bch3)  # Display cp table to find the minimum xerror
optimal_cpbch3 <- fit.bch3$cptable[which.min(fit.bch3$cptable[, "xerror"]), "CP"]
tree_bch3 <- rpart(SC1 ~ . , data = trainb3, method = "class", cp=optimal_cpbch3)
rpart.plot(tree_bch3, type = 4, extra = 100, box.palette="BuGn")

tree_bch3_test <- predict(tree_bch3, newdata = testb3, type = "class")
error_rate_bch3<- mean(tree_bch3_test != testb3$SC1)
print(error_rate_bch3)

set.seed(123)
rf_bch3 <- randomForest(SC1 ~ . , data = trainb3, ntree = 5000)
rf_bch3_test <- predict(rf_bch3, testb3)
importance(rf_bch3)
varImpPlot(rf_bch3)
conf_matrix_rf_bch3 <- table(testb3$SC1, rf_bch3_test)
conf_matrix_rf_table_bch3 <- xtable(conf_matrix_rf_bch3, caption = "Confusion Matrix Random Forest SC1 and BCHIP3")
print(conf_matrix_rf_bch3, caption.placement = "top")
error_rate_bch3_rf <- mean(rf_bch3_test != testb3$SC1)
print(error_rate_bch3_rf)

#SC1 and BCHIP14
fit.bch14 <- rpart(SC1 ~ . , data = tree_df_b14_clean)
printcp(fit.bch14)  # Display cp table to find the minimum xerror
optimal_cpbch14 <- fit.bch14$cptable[which.min(fit.bch14$cptable[, "xerror"]), "CP"]
tree_bch14 <- rpart(SC1 ~ . , data = trainb14, method = "class", cp=optimal_cpbch14)
rpart.plot(tree_bch14, type = 4, extra = 100, box.palette="BuGn")

tree_bch14_test <- predict(tree_bch14, newdata = testb14, type = "class")
error_rate_bch14<- mean(tree_bch14_test != testb14$SC1)
print(error_rate_bch14)

set.seed(123)
rf_bch14 <- randomForest(SC1 ~ . , data = trainb14, ntree = 5000)
rf_bch14_test <- predict(rf_bch14, testb14)
importance(rf_bch14)
varImpPlot(rf_bch14)
conf_matrix_rf_bch14<- table(testb14$SC1, rf_bch14_test)
conf_matrix_rf_table_bch14 <- xtable(conf_matrix_rf_bch14, caption = "Confusion Matrix Random Forest SC1 and BCHIP14")
print(conf_matrix_rf_table_bch14, caption.placement = "top")
error_rate_bch14_rf <- mean(rf_bch14_test != testb14$SC1)
print(error_rate_bch14_rf)

#SC1 and GENEV3
fit.genev3a <- rpart(SC1 ~ . , data = tree_df_g3)
printcp(fit.genev3a)  # Display cp table to find the minimum xerror
optimal_cpgenev3a <- fit.genev3a$cptable[which.min(fit.genev3a$cptable[, "xerror"]), "CP"]
tree_g3 <- rpart(SC1 ~ . , data = traing3, method = "class", cp=optimal_cpgenev3a)
rpart.plot(tree_g3, type = 4, extra = 100, box.palette="BuGn")

tree_g3_test <- predict(tree_g3, newdata = testg3, type = "class")
error_rate_g3<- mean(tree_g3_test != testg3$SC1)
print(error_rate_g3)

set.seed(123)
rf_g3 <- randomForest(SC1 ~ . , data = traing3, ntree = 5000)
rf_g3_test <- predict(rf_g3, testg3)
importance(rf_g3)
varImpPlot(rf_g3)
conf_matrix_rf_g3<- table(testg3$SC1, rf_g3_test)
conf_matrix_rf_table_g3 <- xtable(conf_matrix_rf_g3, caption = "Confusion Matrix Random Forest SC1 and GENEV3")
print(conf_matrix_rf_table_g3, caption.placement = "top")
error_rate_g3_rf <- mean(rf_g3_test != testg3$SC1)
print(error_rate_g3_rf)

#SC1 and GENEV4
fit.genev4 <- rpart(SC1 ~ . , data = tree_df_g4_clean)
printcp(fit.genev4)  # Display cp table to find the minimum xerror
optimal_cpgenev4 <- fit.genev4$cptable[which.min(fit.genev4$cptable[, "xerror"]), "CP"]
tree_g4 <- rpart(SC1 ~ . , data = traing4, method = "class", cp=optimal_cpgenev4)
rpart.plot(tree_g4, type = 4, extra = 100, box.palette="BuGn")

tree_g4_test <- predict(tree_g4, newdata = testg4, type = "class")
error_rate_g4<- mean(tree_g4_test != testg4$SC1)
print(error_rate_g4)

set.seed(123)
rf_g4 <- randomForest(SC1 ~ . , data = traing4, ntree = 5000)
rf_g4_test <- predict(rf_g4, testg4)
importance(rf_g4)
varImpPlot(rf_g4)
conf_matrix_rf_g4<- table(testg4$SC1, rf_g4_test)
conf_matrix_rf_table_g4 <- xtable(conf_matrix_rf_g4, caption = "Confusion Matrix Random Forest SC1 and GENEV4")
print(conf_matrix_rf_table_g4, caption.placement = "top")
error_rate_g4_rf <- mean(rf_g4_test != testg4$SC1)

#------------------------------------------------------------------------------------------------------------------------------------------------
class(techds$F_AGECAT)
class(techds$F_EDUCCAT)
class(techds$F_RELIMP)
class(techds$F_GENDER)
class(techds$F_CREGION)

# Creating tree_df5 with explicit column names
tree_df5 <- data.frame(
  GENEV3 = techds$GENEV3_W99,
  SC1 = techds$SC1,
  F_AGECAT = techds$F_AGECAT,
  F_EDUCCAT = techds$F_EDUCCAT,
  F_RELIMP = techds$F_RELIMP,
  F_GENDER = techds$F_GENDER,
  F_CREGION = techds$F_CREGION,
  F_RELIGCAT1 = techds$F_RELIGCAT1
)

tree_df5_clean <- na.omit(tree_df5[, c("GENEV3", "F_AGECAT", "F_EDUCCAT", "F_RELIMP", "F_GENDER", "SC1", "F_CREGION", "F_RELIGCAT1")])
train_indices_df <- sample(1:nrow(tree_df5_clean), train_size)
traindem <- tree_df5_clean[train_indices_df, ]
testdem <- tree_df5_clean[-train_indices_df, ]

tree.dem <- rpart(GENEV3 ~ . , data = traindem)
summary(tree.dem)
rpart.plot(tree.dem, type = 4, extra = 100, box.palette="BuGn")
tree_dem_test <- predict(tree.dem, newdata = testdem, type = "class")
error_rate_dem<- mean(tree_dem_test != testdem$GENEV3)
print(error_rate_dem)

fit.dem2 <- rpart(GENEV3 ~ F_RELIMP, data = tree_df5_clean)
printcp(fit.dem2)  # Display cp table to find the minimum xerror
optimal_cpdem2 <- fit.dem2$cptable[which.min(fit.dem2$cptable[, "xerror"]), "CP"]
tree_dem2 <- rpart(GENEV3 ~ F_RELIMP , data = traindem, method = "class", cp=optimal_cpdem2)
rpart.plot(tree_dem2, type = 4, extra = 100, box.palette="BuGn")
tree_dem2_test <- predict(tree_dem2, newdata = testdem, type = "class")
error_rate_dem2<- mean(tree_dem2_test != testdem$GENEV3)
print(error_rate_dem2)

fit.dem21 <- rpart(GENEV3 ~ F_RELIGCAT1, data = tree_df5_clean)
printcp(fit.dem21)  # Display cp table to find the minimum xerror
optimal_cpdem21 <- fit.dem21$cptable[which.min(fit.dem2$cptable[, "xerror"]), "CP"]
tree_dem21 <- rpart(GENEV3 ~ F_RELIGCAT1 , data = traindem, method = "class", cp=optimal_cpdem2)
rpart.plot(tree_dem21, type = 4, extra = 100, box.palette="BuGn")
tree_dem21_test <- predict(tree_dem21, newdata = testdem, type = "class")
error_rate_dem21<- mean(tree_dem21_test != testdem$GENEV3)
print(error_rate_dem21)

fit.dem3 <- rpart(GENEV3 ~ F_EDUCCAT, data = tree_df5_clean)
printcp(fit.dem3)  # Display cp table to find the minimum xerror
optimal_cpdem3 <- fit.dem3$cptable[which.min(fit.dem3$cptable[, "xerror"]), "CP"]
tree_dem3 <- rpart(GENEV3 ~ F_EDUCCAT , data = traindem, method = "class", cp=optimal_cpdem3)
rpart.plot(tree_dem3, type = 4, extra = 100, box.palette="BuGn", legend.x=0.65)
tree_dem3_test <- predict(tree_dem3, newdata = testdem, type = "class")
error_rate_dem3<- mean(tree_dem3_test != testdem$GENEV3)
print(error_rate_dem3)

fit.dem4 <- rpart(GENEV3 ~ F_GENDER, data = tree_df5_clean)
printcp(fit.dem4)  # Display cp table to find the minimum xerror
optimal_cpdem4 <- fit.dem4$cptable[which.min(fit.dem4$cptable[, "xerror"]), "CP"]
tree_dem4<- rpart(GENEV3 ~ F_GENDER , data = traindem, method = "class", cp=optimal_cpdem4)
rpart.plot(tree_dem4, type = 4, extra = 100, box.palette="BuGn",legend.x=0.7 )
tree_dem4_test <- predict(tree_dem4, newdata = testdem, type = "class")
error_rate_dem4<- mean(tree_dem4_test != testdem$GENEV3)
print(error_rate_dem4)

fit.dem5 <- rpart(GENEV3 ~ F_CREGION, data = tree_df5_clean)
printcp(fit.dem5)  # Display cp table to find the minimum xerror
optimal_cpdem5 <- fit.dem5$cptable[which.min(fit.dem5$cptable[, "xerror"]), "CP"]
#tree.dem3 <- tree(SC1 ~ F_CREGION, data = tree_df5_clean)
tree_dem5 <- rpart(SC1 ~ F_CREGION , data = traindem, method = "class", cp=0.0001)
rpart.plot(tree_dem5, type = 4, extra = 100, box.palette="BuGn")
tree_dem5_test <- predict(tree_dem5, newdata = testdem, type = "class")
error_rate_dem5<- mean(tree_dem5_test != testdem$SC1)
print(error_rate_dem5)
#-------------------------------------------------------------------------------------------------------
#PCA Analysis
#STEP 1: I'll create a new data frame with jsut the variables of interest -- since we are looking at the effect of BCHIP and GENEV questions on
# an individual's attitude towards technology, I will use all of the dummy variables Dagmar made in part 1.
step_1 <- data.frame(all_dummy$BCHIP14_a_W99_dummy, all_dummy$BCHIP14_b_W99_dummy, 
                     all_dummy$BCHIP14_c_W99_dummy, all_dummy$BCHIP14_d_W99_dummy,
                     all_dummy$GENEV3_W99_dummy, 
                     all_dummy$BCHIP3_W99_dummy,
                     all_dummy$GENEV4_a_W99_dummy, all_dummy$GENEV4_b_W99_dummy,
                     all_dummy$GENEV4_c_W99_dummy, all_dummy$GENEV4_d_W99_dummy)

#1.A: ISLR insrtucts us to examine the mean and variances of our columns. First I will drop the NA responses. Then I will calculate and compare the means
# and variances for each column in the new dataframe.
#This step allows us to see the differences in mean and variance, and emphasizes the importance of standardization through z-transformation.
#We can see that leaving the variables untransformed would skew our results in favor of variables with higher variance or mean, like 
#BCHIP14_d_W99_dummy or BCHIP14_d_W99_dummy.

if (!require(tidyr)) install.packages("tidyr")
library(tidyr)


step_2 <- drop_na(step_1)
mean_check <- apply(step_2, 2, mean)
var_check <- apply(step_2, 2, var)
sd_check <- apply(step_2, 2, sd)
sort(mean_check, decreasing = TRUE)
sort(var_check, decreasing = TRUE)
sort(sd_check, decreasing = TRUE)

#1.B: Now we will use the prcomp() function to perform principal components analysis for the new dataframe. 
#From ISLR, pg 533:
#"By default, the prcomp() function centers the variables to have mean zero. By using the option scale = TRUE, we scale the variables to 
#have a standard deviation of 1."
#We can examine the output of PCA by calling out the output of the function (use names() to determine the output feature you'd like to see).
pca <- prcomp(step_2, scale = TRUE)

names(pca) #Output feature names
pca$center #These are the means of the columns pre-scaling -- they are the same as the mean_check results above.
pca$scale #These are the standard deviations of the columsn pre-scaling -- they are the same as the sd_check results above.

#1.C: From ISLR, pg 533:
# The rotation matrix provides the principal componenet loadings; each column of pca$rotation contains the corresponding principal component loading vector.
# From ChatGPT (lol): The magnitude of the loading indicates the strength of the relationship between the original variable and the principal component. Larger
#absolute values suggest a stronger relationship. The sign of the loading indicated the direction of the relationship. Positive loadings indicate a positive corrrelation,
# negative loading indicate a negative correlation.
#Also from ChatGPT (forgive me): PC1 is like the main direction or trend in the data. It's what expalins the largest amount of variation between data points.
#PC2 is the next big trend that explains variation that PC1 didn't capture.
#install.packages("xtable")
library(xtable)

pca$rotation
# Load the xtable package
library(xtable)

# Assuming pca$rotation is the rotation matrix from PCA
# Convert pca$rotation to a data frame (assuming it's a matrix)
rotation_df <- as.data.frame(pca$rotation)

# Print the data frame (you can use print(rotation_df) to see the structure)
rotation_df

# Convert the data frame to a LaTeX table
rotation_table <- xtable(rotation_df)

# Print the LaTeX-formatted table
print(rotation_table)


#1.D: Next, I'll find the standard deviation, variance, and proportion of variance for each column in the PCA results.
#In our pca_pve we can see that PC1 explains ~41% of the variance in the data, PC2 explains about 17% of the variance in the data, and PC3 explains about 10% of
#the variance in the data (total ~69% in PC1-3).
pca$sdev #Standard deviation
pca_var <- pca$sdev^2 #Variance
pca_var
pca_pve <- pca_var/sum(pca_var) #Proportion of variance explained
pca_pve

# Load necessary library
library(ggplot2)

# Create a data frame for plotting
pca_data <- data.frame(PC = 1:length(pca_pve),
                       PVE = pca_pve,
                       Cumulative_PVE = cumsum(pca_pve))

# Create the scree plot with gradient colors and no grid
pca_plot <- ggplot(pca_data, aes(x = factor(PC), y = PVE)) +
  geom_bar(stat = "identity", aes(fill = PVE), width = 0.7) +
  geom_line(aes(y = Cumulative_PVE, group = 1), color = "darkred", linewidth = 1.5) +
  geom_point(aes(y = Cumulative_PVE), color = "darkred", size = 3) +
  geom_text(aes(y = Cumulative_PVE + 0.02, label = scales::percent(Cumulative_PVE, accuracy = 0.1)), vjust = 0, color = "darkred") +
  scale_fill_gradient(low = "blue", high = "red") +  # Gradient color from blue to red
  labs(title = "PCA Scree Plot",
       x = "Principal Component",
       y = "Proportion of Variance Explained") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(),  # Remove major grid
        panel.grid.minor = element_blank())  # Remove minor grid

# Print the plot
print(pca_plot)
