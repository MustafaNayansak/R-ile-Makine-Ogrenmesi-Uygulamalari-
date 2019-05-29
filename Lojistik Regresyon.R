# Kategorik Veri Analizi 

setwd("C:/Users/Asus/Desktop/Sixth Episode")


# Veri Okuma Ýþlemi
veri <- read.table(file = "uis.dat",header = F)
str(veri)
veri <- veri[,-1]
a <- c("AGE","BECK","IVHX","NDRUGTX","RACE","TREAT","SITE","DFREE")
colnames(veri) <- a
head(veri)
str(veri)
attach(veri)


# Modeli Kurma Ýþlemi
library(caret)
model<-glm(DFREE~AGE+BECK+factor(IVHX)+NDRUGTX+
             RACE+TREAT+SITE,family = binomial)
summary(model)


# Modelin Anlamlýlýðýný Ýnceleme
k1<-(model$df.null-model$df.residual) 
table<-qchisq(0.95,k1) 
g.full<-model$null.deviance-model$deviance 
if(g.full>table) 
{
  print("H0 Red, %95 güvenle model anlamlýdýr.") 
}else{
  print("H0 Reddedilemez, model %95 güvenle anlamlý deðildir.")
}


# Deðiþkenlerin Anlamlýlýklarýný Ýnceleme(Yaþ Deðiþkeni)
b<-0.0504143
st<-0.0174057
wald<-b/st
if(wald>abs(qnorm(0.025)))
{
  print("H0 hipotezi reddedilir. Yaþ deðiþkeni, tedavi programý sonunda hastanýn durumu üzerinde anlamlý bir katkýsý vardýr.")
}else{
  print("H0 Reddedilemez, seçilen deðiþken %95 güvenle anlamlý deðildir.")
}


# Yeni Modeli Kurma(Anlamsýz Deðiþkenleri Çýkarma)
model1<-glm(DFREE~AGE+factor(IVHX)+NDRUGTX+
              +TREAT,family = binomial)
summary(model1)


# Yeni Modelin Anlamlýlýðýný Test Etme
k2<-(model1$df.null-model1$df.residual) 
table<-qchisq(0.95,k2) 
g.azalt<-model1$null.deviance-model1$deviance 
if(g.azalt<table) 
{
  print("H0 Red, %95 güvenle model anlamlýdýr.") 
}else{
  print("H0 Reddedilemez, model %95 güvenle anlamlý deðildir.")
}


# Hangi Modelin Anlamlý Olduðunu Belirleme
g3<-model1$deviance-model$deviance
k.new<-k1-k2
table1<-qchisq(0.95,k.new) 
if(g3>k.new) 
{
  print("H0 Red, En az bir deðiþken sýfýrdan farklýdýr ve full model kullanýlmalýdýr.") 
} else{
  print("H0 Reddedilemez, modelden çýkarýlan deðiþkenler bu model için anlamsýzdýr")
}


# Belirli Özelliklere Sahip Bir Bireyin Sonuç Deðiþkeni Tahmini
gx<-(-2.3376+33*0.05258-1*0.62366-3*0.06376+1*0.45134)
gx
p<-exp(gx)/(1+exp(gx))
p


# Sensitivity ve Specificity Deðerleri
y_fitnew<-c(veri)
y_fit<-model$fitted.values
cut<-0.5
y_fitnew<-sapply(1:length(y_fit),function(i)
{
  if(y_fit[i]<cut)
  {
    y_fitnew[i]=0
  }
  else
  {
    y_fitnew[i]=1
  }
}
)
ctable<-table(y_fitnew,veri$DFREE)
ctable

sensitivity<-ctable[2,2]/(ctable[2,1]+ctable[2,2])
specificity<-ctable[1,1]/(ctable[1,1]+ctable[1,2])
sensitivity
specificity

# Modelin Tahmin Performansýný Ölçmek için ROC eðrisi
library(ROSE)

nrow(veri)
table(veri$DFREE)

basari <- veri[veri$DFREE==1,]
basarisizlik <- veri[veri$DFREE==0,]

x <- nrow(basari) / nrow(basarisizlik)  #Baþarý ile Baþarýsýzlýk satýrlarýnýn arasýndaki kat oraný

deneme.veri.evet <- basari[sample(nrow(basari),73),]  
deneme.veri.hayir <- basarisizlik[sample(nrow(basarisizlik),73/x),]  
model.veri <- rbind(deneme.veri.evet,deneme.veri.hayir)
nrow(model.veri) 

tahmin.veri.evet <- basari[sample(nrow(basari),73),]  
tahmin.veri.hayir <- basarisizlik[sample(nrow(basarisizlik), 73/x),]   
tahmin.veri <- rbind(tahmin.veri.evet, tahmin.veri.hayir)
nrow(tahmin.veri)  

# Model kurmak için oluþturduðumuz veri setini eðitiyoruz
model3 <- glm(model.veri$DFREE ~
                model.veri$AGE +
                model.veri$IVHX +
                model.veri$NDRUGTX +
                model.veri$TREAT,
              family = binomial)

summary(model3)

model.tahmin <- predict(model3,tahmin.veri)
roc.curve(tahmin.veri$DFREE,model.tahmin)



