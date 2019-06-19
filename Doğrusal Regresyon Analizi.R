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

print("Çalýþmanýn Türü : Doðrusal Regresyon Analizi")

print("Çalýþmanýn Amacý : Bir binanýn saðlamlýðý üzerinde
      etkili olduðu düþünülen baðýmsýz deðiþkenler ile bir 
      regresyon modeli kurmak, kurulan modelin ve deðiþkenlerin 
      anlamlýlýklarýný test etmek, en iyi modeli bulmak, 
      kurulan modelin tahmin performansýný ölçmek ve tüm bu
      süreci özetlemek. ")

print("Makine Öðrenmesi Nedir? : Bilgisayarlarý, insan gibi 
      düþünebilen ve analiz edebilen bir yapý haline getirmek 
      için, istatistiksel yöntemlere dayanan öðretme 
      tekniklerinin tamamýdýr. Bu þekilde, insanlar analiz 
      edemeyeceði miktardaki büyük ve karmaþýk haldeki veriyi 
      analiz edebilir hale gelmektedir.")

print("Doðrusal Regresyon : Bir baðýmlý deðiþken ile bir veya 
      birden fazla baðýmsýz deðiþkenin arasýndaki doðrusal i
      liþkiyi modellemeye yarayan istatistiksel bir yöntemdir.")

# VERÝNÝN OKUNMASI
df <- read_xlsx("C:/Users/Asus/Desktop/R ile ML Uygulamalarý/Doðrusal Regresyon/data.xlsx")
head(df)

# VERÝ ÖN ÝÞLEME

## Deðiþken Dönüþtürme
saglamlik <- as.numeric(df$Saglamlýk)
cimento <- as.numeric(df$Cimento)
mucur <- as.numeric(df$Mucur)
kul<- as.numeric(df$Kül)
kaba_agr<- as.numeric(df$`Kaba Agr`)
ince_agr<- as.numeric(df$`Ýnce Agr`)

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

## Baðýmlý Deðiþken için Aykýrý Gözlem Tespiti
summary(saglamlik)
hist(x = veri$saglamlik,freq = T)
veri <- veri[-which(saglamlik>60),]



# KEÞÝFÇÝ VERÝ ANALÝZÝ
## Veri setinin genel yapýsý hakkýnda bilgiler için glimpse() fonksiyonu
glimpse(veri)

## Verideki sürekli deðiþkenlerin özet istatistikleri için profiling_num()
profiling_num(veri)

## Deðiþkenler arasýndaki kolerasyonu incelemek için aþaðýdaki fonksiyon kullanýlýr. 
chart.Correlation(veri)

## Seçilen deðiþkenlerin baðýmlý deðiþkenle arasýndaki iliþki için featurePlot() fonksiyonu
names(veri)
featurePlot(x = veri[,c("cimento", "kaba_agr", "kul")], y =veri$saglamlik)

# DOÐRUSAL REGRESYON MODELÝ

## Model
model <- lm(veri$saglamlik~
              veri$cimento+
              veri$mucur+
              veri$kul+
              veri$kaba_agr+
              veri$ince_agr)
summary(model)
## Katsayýlarýn Anlamlýlýðý 
# confint() fonksiyonu yardýmý ile modele eklenen deðiþkenlerin anlamlýlýklarýný incelemek mümkündür. 
# Bu fonksiyon bir sayý aralýðý döndürür ve bu aralýðýn 0 deðerini içermemesi beklenir. 
# Eðer 0 deðerini içeriyorsa deðiþken o güven düzeyinde anlamsýzdýr yorumu yapýlabilir.
confint(model)         

## Model Varsayýmlarý 
par(mfrow=c(2,2))
plot(model) 
ols_plot_resid_lev(model)

# 1. Grafik için; Varyans homojenliði var mý yok mu diye bakýyoruz. Noktalarýn 0 etrafýnda rasgele daðýlmasý istenir - gözlem sayýsýnýn az olmasý sebebi ile grafikler subjektiftir, yapýlan yorumlar yanlýdýr.
# 2. Grafik için; Artýklarýn normal daðýlýp daðýlmadýðýný belirtir. Bir doðru üzerinde olmasý istenir. Görsel olarak normal daðýlýyor denilebilir ama test yapýlmasý gerekmektedir.
# 3. Grafik için; Standartlaþtýrýlmýþ artýk deðerler için ve fitted valuelar için inceliyoruz. 1. grafikle benzer yapýdalar.
# 4. Grafik için; Standardizied residuals kýsmý uç deðerlerin veya aykýrý gözlem etrafýnda olup olmadýðýný gösterir. Standardized gözlemlerin +3 ve -3 deðerleri arasýnda olup olmadýðý incelenir.
# 4. Grafik için; Leverage ile etkin gözlem olup olmadýðýna bakýyoruz.- Leverege için kriter 2*p/n. Hesaplanan deðer bu kriteri geçiyor ise etkin gözlemdir. Yani modelin baþarýsýný doðrudan etkiler yorumu yapýlabilir.

### Baðýmlý Deðiþkenin Yayýlýmý Hakkýnda
ols_plot_response(model)

### Normallik Testi
ols_test_normality(model)

### Varyans Homojenliði Testi
ols_test_breusch_pagan(model)

### Çoklu Doðrusal Baðýntý Problemi
ols_coll_diag(model)
# Tolerans = 1-p(x,y)^2
# VIF = 1/Tolerans
# VIF deðerinin 10 deðerinden küçük olmasý beklenir

# STEPWISE YONTEMLER
model <- lm(saglamlik~.,data = veri)
a <- ols_step_all_possible(model)
a 
data.frame(a$predictors,a$adjr)

# Ýleri yönlü
ols_step_forward_aic(model)

# Geri Yönlü
ols_step_backward_aic(model)

# Çift Yönlü
ols_step_both_aic(model)

# YENÝ MODELÝN KURULMASI
model1 <- lm(saglamlik~cimento+kul,data = veri)
summary(model1)

## Tahmin
head(predict(model1))

set.seed(123)

## Aþaðýdaki deðerlere sahip bir gözlem için nokta tahmini
new_obs <- data.frame(cimento=340, kul=180)
predict(model1, new_obs)

## Aþaðýdaki deðerlere sahip bir gözlem için aralýk tahmini
predict(model1, new_obs, interval = "confidence")

print("Tahmin edilen deðerler ile gerçek deðerler arasýndaki 
      fark 'Hata' olarak bilinir ve bu hata 
      üzerinden hesaplanan deðerler ile modelin tahmin 
      performansý ölçülür")

## Artýklar
head(resid(model1))
#Standartlaþtýrýlmýþ Artýklar
head(rstudent(model1))
#Gerçek deðerler
head(veri$saglamlik)
#tahmin edilen deðerler
head(predict(model1))
#Tahmin edilen ile gerçek deðerin karþýlaþtýrýlmasý
kar <- data.frame(
  y <- head(veri$saglamlik),
  ysapka <- head(predict(model1))
)

kar$hata <- kar$y....head.veri.saglamlik.-kar$ysapka....head.predict.model1..
kar$kare <- kar$hata^2
mse <- mean(kar$kare)
print("Kare alma iþlemi ile hatalarýn þiddeti de artýyor ve - 
      deðerlerde nötrleniyor. Kare alma iþleminden
      sonra karelerin kökü alýnarak deðerler küçültülebilir.
      Bunun adýna Da RMSE deðeri denir.")
rmse <- sqrt(mean(kar$kare))

# ANOVA
anova(model1)
print("ANOVA'da kullanýlan F istatistiði ile modelin anlamlýlýðý
      ölçülebilir")
print("RKare : Bir baðýmlý deðiþkenin, baðýmsýz deðiþkenlerce
      ne kadar açýklanabildiðini gösteren
      bir istatistiktir.")


# Test ve Train Verisi Ayýrma Ýþlemi
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
# times 1 olmasý 1 kez çekeceðini söyler

train <- veri_yeni[train_indeks,]
test <- veri_yeni[-train_indeks,]

# Sadece train ve test olarak 2ye ayýrmak yetmeyebilir o yüzden X ve Y deðerlerini ayrý olarak seçmek gerekir.

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

print("Yukarýdaki fonksiyon ile baðýmlý deðiþken ve 
      baðýmsýz deðiþeknlerin arasýndaki iliþkiyi göstermek
      dýþýnda, baðýmsýz deðiþkenlerin kendi arasýndaki
      doðrusal iliþkiyi de gösteriyor. Çoklu doðrusal 
      baðlantýyý bu þekilde görsel olarak ve kolerasyon 
      deðerlerine bakarak inceleyebiliriz")

model2 <- lm(Saglamlik~kul+cimento, data = training)
# Eðitim Hatasýnýn Hesaplanmasý
# defaultsummary() fonksiyonunu kullanmak için aþaðýdaki taslak oluþturulmalýdýr.
defaultSummary(data.frame(obs = training$Saglamlik,
                          pred = model2$fitted.values))
defaultSummary(data.frame(obs = test_y,
                          pred = predict(model2, test_x)))

print("Verdiðimiz modeli kullanarak test seti olarak 
ayýrdýðýmýz baðýmsýz deðiþken seti ile bir tahmin 
yapacak ve pred e atayacak")

ctrl <- trainControl(method = "cv", number = 10)
model_val <- train(x = train_x, y = train_y,
                   method = "lm",
                   trControl = ctrl)
summary(model_val)
names(model_val)
model_val$metric

model_val$results
