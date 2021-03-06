library(caret)
library(tidyverse)
library(AppliedPredictiveModeling)
library(pls)
library(elasticnet)
library(broom)
library(glmnet)
library(MASS)
library(ISLR)
library(PerformanceAnalytics)
library(funModeling)
library(Matrix)
library(readxl)
library(olsrr)

print("�al��man�n T�r� : Do�rusal Regresyon Analizi")

print("�al��man�n Amac� : Bir binan�n sa�laml��� �zerinde
      etkili oldu�u d���n�len ba��ms�z de�i�kenler ile bir 
      regresyon modeli kurmak, kurulan modelin ve de�i�kenlerin 
      anlaml�l�klar�n� test etmek, en iyi modeli bulmak, 
      kurulan modelin tahmin performans�n� �l�mek ve t�m bu
      s�reci �zetlemek. ")

print("Makine ��renmesi Nedir? : Bilgisayarlar�, insan gibi 
      d���nebilen ve analiz edebilen bir yap� haline getirmek 
      i�in, istatistiksel y�ntemlere dayanan ��retme 
      tekniklerinin tamam�d�r. Bu �ekilde, insanlar analiz 
      edemeyece�i miktardaki b�y�k ve karma��k haldeki veriyi 
      analiz edebilir hale gelmektedir.")

print("Do�rusal Regresyon : Bir ba��ml� de�i�ken ile bir veya 
      birden fazla ba��ms�z de�i�kenin aras�ndaki do�rusal i
      li�kiyi modellemeye yarayan istatistiksel bir y�ntemdir.")

# VER�N�N OKUNMASI
df <- read_xlsx("C:/Users/Asus/Desktop/R ile ML Uygulamalar�/Do�rusal Regresyon/data.xlsx")
head(df)

# VER� �N ��LEME

## De�i�ken D�n��t�rme
saglamlik <- as.numeric(df$Saglaml�k)
cimento <- as.numeric(df$Cimento)
mucur <- as.numeric(df$Mucur)
kul<- as.numeric(df$K�l)
kaba_agr<- as.numeric(df$`Kaba Agr`)
ince_agr<- as.numeric(df$`�nce Agr`)

veri <- data.frame(
  saglamlik,
  cimento,
  mucur,
  kul,
  kaba_agr,
  ince_agr
)

## Eksik Verilerin Tespit Edilmesi
sum(is.na(veri))

## Ba��ml� De�i�ken i�in Ayk�r� G�zlem Tespiti
summary(saglamlik)
hist(x = veri$saglamlik,freq = T)
veri <- veri[-which(saglamlik>60),]



# KE��F�� VER� ANAL�Z�
## Veri setinin genel yap�s� hakk�nda bilgiler i�in glimpse() fonksiyonu
glimpse(veri)

## Verideki s�rekli de�i�kenlerin �zet istatistikleri i�in profiling_num()
profiling_num(veri)

## De�i�kenler aras�ndaki kolerasyonu incelemek i�in a�a��daki fonksiyon kullan�l�r. 
chart.Correlation(veri)

## Se�ilen de�i�kenlerin ba��ml� de�i�kenle aras�ndaki ili�ki i�in featurePlot() fonksiyonu
names(veri)
featurePlot(x = veri[,c("cimento", "kaba_agr", "kul")], y =veri$saglamlik)

# DO�RUSAL REGRESYON MODEL�

## Model
model <- lm(veri$saglamlik~
              veri$cimento+
              veri$mucur+
              veri$kul+
              veri$kaba_agr+
              veri$ince_agr)
summary(model)
## Katsay�lar�n Anlaml�l��� 
# confint() fonksiyonu yard�m� ile modele eklenen de�i�kenlerin anlaml�l�klar�n� incelemek m�mk�nd�r. 
# Bu fonksiyon bir say� aral��� d�nd�r�r ve bu aral���n 0 de�erini i�ermemesi beklenir. 
# E�er 0 de�erini i�eriyorsa de�i�ken o g�ven d�zeyinde anlams�zd�r yorumu yap�labilir.
confint(model)         

## Model Varsay�mlar� 
par(mfrow=c(2,2))
plot(model) 
ols_plot_resid_lev(model)

# 1. Grafik i�in; Varyans homojenli�i var m� yok mu diye bak�yoruz. Noktalar�n 0 etraf�nda rasgele da��lmas� istenir - g�zlem say�s�n�n az olmas� sebebi ile grafikler subjektiftir, yap�lan yorumlar yanl�d�r.
# 2. Grafik i�in; Art�klar�n normal da��l�p da��lmad���n� belirtir. Bir do�ru �zerinde olmas� istenir. G�rsel olarak normal da��l�yor denilebilir ama test yap�lmas� gerekmektedir.
# 3. Grafik i�in; Standartla�t�r�lm�� art�k de�erler i�in ve fitted valuelar i�in inceliyoruz. 1. grafikle benzer yap�dalar.
# 4. Grafik i�in; Standardizied residuals k�sm� u� de�erlerin veya ayk�r� g�zlem etraf�nda olup olmad���n� g�sterir. Standardized g�zlemlerin +3 ve -3 de�erleri aras�nda olup olmad��� incelenir.
# 4. Grafik i�in; Leverage ile etkin g�zlem olup olmad���na bak�yoruz.- Leverege i�in kriter 2*p/n. Hesaplanan de�er bu kriteri ge�iyor ise etkin g�zlemdir. Yani modelin ba�ar�s�n� do�rudan etkiler yorumu yap�labilir.

### Ba��ml� De�i�kenin Yay�l�m� Hakk�nda
ols_plot_response(model)

### Normallik Testi
ols_test_normality(model)

### Varyans Homojenli�i Testi
ols_test_breusch_pagan(model)

### �oklu Do�rusal Ba��nt� Problemi
ols_coll_diag(model)
# Tolerans = 1-p(x,y)^2
# VIF = 1/Tolerans
# VIF de�erinin 10 de�erinden k���k olmas� beklenir

# STEPWISE YONTEMLER
model <- lm(saglamlik~.,data = veri)
a <- ols_step_all_possible(model)
a 
data.frame(a$predictors,a$adjr)

# �leri y�nl�
ols_step_forward_aic(model)

# Geri Y�nl�
ols_step_backward_aic(model)

# �ift Y�nl�
ols_step_both_aic(model)

# YEN� MODEL�N KURULMASI
model1 <- lm(saglamlik~cimento+kul,data = veri)
summary(model1)

## Tahmin
head(predict(model1))

set.seed(123)

## A�a��daki de�erlere sahip bir g�zlem i�in nokta tahmini
new_obs <- data.frame(cimento=340, kul=180)
predict(model1, new_obs)

## A�a��daki de�erlere sahip bir g�zlem i�in aral�k tahmini
predict(model1, new_obs, interval = "confidence")

print("Tahmin edilen de�erler ile ger�ek de�erler aras�ndaki 
      fark 'Hata' olarak bilinir ve bu hata 
      �zerinden hesaplanan de�erler ile modelin tahmin 
      performans� �l��l�r")

## Art�klar
head(resid(model1))
#Standartla�t�r�lm�� Art�klar
head(rstudent(model1))
#Ger�ek de�erler
head(veri$saglamlik)
#tahmin edilen de�erler
head(predict(model1))
#Tahmin edilen ile ger�ek de�erin kar��la�t�r�lmas�
kar <- data.frame(
  y <- head(veri$saglamlik),
  ysapka <- head(predict(model1))
)

kar$hata <- kar$y....head.veri.saglamlik.-kar$ysapka....head.predict.model1..
kar$kare <- kar$hata^2
mse <- mean(kar$kare)
print("Kare alma i�lemi ile hatalar�n �iddeti de art�yor ve - 
      de�erlerde n�trleniyor. Kare alma i�leminden
      sonra karelerin k�k� al�narak de�erler k���lt�lebilir.
      Bunun ad�na Da RMSE de�eri denir.")
rmse <- sqrt(mean(kar$kare))

# ANOVA
anova(model1)
print("ANOVA'da kullan�lan F istatisti�i ile modelin anlaml�l���
      �l��lebilir")
print("RKare : Bir ba��ml� de�i�kenin, ba��ms�z de�i�kenlerce
      ne kadar a��klanabildi�ini g�steren
      bir istatistiktir.")


# Test ve Train Verisi Ay�rma ��lemi
saglamlik <- veri$saglamlik
cimento <- veri$cimento
kul <- veri$kul

veri_yeni <- data.frame(
  saglamlik,
  cimento,
  kul
)

set.seed(1234)

train_indeks <- createDataPartition(veri_yeni$saglamlik,
                                    p = .8, 
                                    list = FALSE, 
                                    times = 1)
# times 1 olmas� 1 kez �ekece�ini s�yler

train <- veri_yeni[train_indeks,]
test <- veri_yeni[-train_indeks,]

# Sadece train ve test olarak 2ye ay�rmak yetmeyebilir o y�zden X ve Y de�erlerini ayr� olarak se�mek gerekir.

train_x <- train %>% 
  dplyr::select(-saglamlik)
train_y <- train$saglamlik

test_x <- test %>% 
  dplyr::select(-saglamlik)
test_y <- test$saglamlik

training <- data.frame(train_x, Saglamlik = train_y)
glimpse(training)
plot_num(training)

pairs(veri_yeni %>% 
        dplyr::select(c("saglamlik","kul","cimento")))

chart.Correlation(veri_yeni %>%
                    dplyr::select(c("saglamlik","kul","cimento")))

print("Yukar�daki fonksiyon ile ba��ml� de�i�ken ve 
      ba��ms�z de�i�eknlerin aras�ndaki ili�kiyi g�stermek
      d���nda, ba��ms�z de�i�kenlerin kendi aras�ndaki
      do�rusal ili�kiyi de g�steriyor. �oklu do�rusal 
      ba�lant�y� bu �ekilde g�rsel olarak ve kolerasyon 
      de�erlerine bakarak inceleyebiliriz")

model2 <- lm(Saglamlik~kul+cimento, data = training)
# E�itim Hatas�n�n Hesaplanmas�
# defaultsummary() fonksiyonunu kullanmak i�in a�a��daki taslak olu�turulmal�d�r.
defaultSummary(data.frame(obs = training$Saglamlik,
                          pred = model2$fitted.values))
defaultSummary(data.frame(obs = test_y,
                          pred = predict(model2, test_x)))

print("Verdi�imiz modeli kullanarak test seti olarak 
ay�rd���m�z ba��ms�z de�i�ken seti ile bir tahmin 
yapacak ve pred e atayacak")

ctrl <- trainControl(method = "cv", number = 10)
model_val <- train(x = train_x, y = train_y,
                   method = "lm",
                   trControl = ctrl)
summary(model_val)
names(model_val)
model_val$metric

model_val$results
