# PCR-Principal Component Regression
'
Temel bileşen analizi elimizdeki p sayıdaki bilginin yüksek kısmını, ondan daha az değişkenle açıklamak fikrine dayanır
Büyük veri analiz süreçlerinin en zor kısmı veride çok fazla değişkenin olması anlamına geliyor.
Veride çok fazla değişken olması multicollinearity adında bir problem ortaya çıkarıyor.
Multicollinearity bağımsız değişkenler arasında yüksek kolerasyonlar oluşturması anlamına geliyor

Temel bileşenler, bağımsız değişkenlere göre alt çarpanlara indirgenmiş ve birbirinden bağımsız olan değişkenlerdir
Bu bileşenler ile kurulan regresyon modeline ise Temel Bileşen Regresyonu denir
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
# Bileşenler ve bu bileşenlerin varyans açıklamaları ile ilgili bilgiler vardır

validationplot(pcr_fit, val.type = "MSEP")
# Temel bileşenler için hesaplanacak olaan hata metriği

names(pcr_fit)

# Eğitim Hatası
defaultSummary(data.frame(obs = training$Salary,
                          pred = as.vector(pcr_fit$fitted.values))
)


# Tahmin
predict(pcr_fit,test_x)
# Her bileşen için tahminler yapar

# 2 bileşen için
predict(pcr_fit,test_x[1:5,], ncomp = 1:2)

# Test Hatası
defaultSummary(data.frame(obs = test_y,
                          pred = as.vector(predict(pcr_fit, test_x, ncomp = 1:2)))
)


# Model Tuning 
# Tune edilecek parametre bileşen sayısıdır

ctrl <- trainControl(method = "CV",
                     number = 10)

set.seed(100)
pcr_tune <- train(train_x, train_y,
                  method = "pcr",
                  trainControl = ctrl,
                  preProc = c("center", "scale"))
# preproc veri setinde ölçeklendirme, dönüştürme işlemi yapmak için kullanılır

pcr_tune

pcr_tune <- train(train_x, train_y,
                  method = "pcr",
                  trainControl = ctrl,
                  tuneLength = 20,
                  preProc = c("center", "scale"))
# tuneLength ile max 20 değişkene kadar dene

pcr_tune

plot(pcr_tune)

pcr_tune$finalModel
pcr_tune$bestTune

defaultSummary(data.frame(obs = test_y,
                          pred = as.vector(predict(pcr_tune, test_x)))
)
