# PCR-Principal Component Regression
'
Temel bileþen analizi elimizdeki p sayýdaki bilginin yüksek kýsmýný, ondan daha az deðiþkenle açýklamak fikrine dayanýr
Büyük veri analiz süreçlerinin en zor kýsmý veride çok fazla deðiþkenin olmasý anlamýna geliyor.
Veride çok fazla deðiþken olmasý multicollinearity adýnda bir problem ortaya çýkarýyor.
Multicollinearity baðýmsýz deðiþkenler arasýnda yüksek kolerasyonlar oluþturmasý anlamýna geliyor

Temel bileþenler, baðýmsýz deðiþkenlere göre alt çarpanlara indirgenmiþ ve birbirinden baðýmsýz olan deðiþkenlerdir
Bu bileþenler ile kurulan regresyon modeline ise Temel Bileþen Regresyonu denir
'
library(caret)
library(tidyverse)
library(pls)
library(AppliedPredictiveModeling)
library(elasticnet)
library(broom)
library(glmnet)
library(MASS)
library(ISLR)
library(PerformanceAnalytics)
library(funModeling)
library(Matrix)

df <- Hitters
df <- na.omit(df)

glimpse(df)
rownames(df) <- c()

set.seed(3456)
train_indeks <- createDataPartition(df$Salary,
                                    p = 0.8,
                                    list = FALSE,
                                    times = 1)

train <- df[train_indeks,]
test <- df[-train_indeks,]

train_x <- train %>% dplyr::select(-Salary)
train_y <- train$Salary

test_x <- test %>% dplyr::select(-Salary)
test_y <- test$Salary

training <- data.frame(train_x, Salary = train_y)

# Model
pcr_fit <- pcr(Salary~.,data = training,
    scale = TRUE,
    validation = "CV")
summary(pcr_fit)
# Bileþenler ve bu bileþenlerin varyans açýklamalarý ile ilgili bilgiler vardýr

validationplot(pcr_fit, val.type = "MSEP")
# Temel bileþenler için hesaplanacak olaan hata metriði

names(pcr_fit)

# Eðitim Hatasý
defaultSummary(data.frame(obs = training$Salary,
                          pred = as.vector(pcr_fit$fitted.values))
)


# Tahmin
predict(pcr_fit,test_x)
# Her bileþen için tahminler yapar

# 2 bileþen için
predict(pcr_fit,test_x[1:5,], ncomp = 1:2)

# Test Hatasý
defaultSummary(data.frame(obs = test_y,
                          pred = as.vector(predict(pcr_fit, test_x, ncomp = 1:2)))
)


# Model Tuning 
# Tune edilecek parametre bileþen sayýsýdýr

ctrl <- trainControl(method = "CV",
                     number = 10)

set.seed(100)
pcr_tune <- train(train_x, train_y,
                  method = "pcr",
                  trainControl = ctrl,
                  preProc = c("center", "scale"))
# preproc veri setinde ölçeklendirme, dönüþtürme iþlemi yapmak için kullanýlýr

pcr_tune

pcr_tune <- train(train_x, train_y,
                  method = "pcr",
                  trainControl = ctrl,
                  tuneLength = 20,
                  preProc = c("center", "scale"))
# tuneLength ile max 20 deðiþkene kadar dene

pcr_tune

plot(pcr_tune)

pcr_tune$finalModel
pcr_tune$bestTune

defaultSummary(data.frame(obs = test_y,
                          pred = as.vector(predict(pcr_tune, test_x)))
)
