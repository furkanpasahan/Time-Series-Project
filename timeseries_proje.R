install.packages("pastecs")
library(pastecs)
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("fpp2")
library(fpp2)
install.packages("Kendall")
library(Kendall)
install.packages("trend")

library(trend)
install.packages("seastests")
library(seastests)
install.packages("tseries")
library(tseries)
install.packages("forecast")
library(forecast)
install.packages("car")
library(car)
install.packages("lmtest")
library(lmtest)
install.packages("FSA")
library(FSA)
install.packages("stats")
library(stats)
install.packages("SciViews")
library("SciViews")
require(readxl)
datam<-readxl::read_excel(file.choose())


ts_data <- ts(data = datam$Value, start = c(2005, 01), end = c(2022, 12), frequency = 12)
ts_data
plot.ts(ts_data)
autoplot(ts_data,xlab="Yillar",lwd = 1, col="blue",main = "Eğlence ve Sanat Sektöründe İş Başvuruları ") 
#trend gözlemliyoruz aynı zamanda mevsimsellik olduğunu görüyoruz. Çarpımsal modeldir.

seasonplot(ts_data)
stat.desc(ts_data) #özet istatistikler

hist(ts_data,main="Verinin Dağılımı") #verinin dağılımı sağa çarpık normal dagılmadığını söyleyebiliriz.

isSeasonal(ts_data, test = "combined", freq = 12)#aylık veri olduğu için freq=12
#mevsimsellik vardır

adf.test(ts_data) # Durağan değildir


once<-ts_data[1:108]
sonra<-ts_data[109:216]

t.test(once,sonra) #böldüğümüz verinin ortalamalarını kıyasladık

var.test(once,sonra) #varyanslar arasında farklılık vardır.

boxplot(once,sonra, main = "Aylık Başvuru Sayısı",
        xlab = "Zaman",
        ylab = "Başvuru Sayısı",
        col = "red",
        border = "black",
        horizontal = TRUE,
        notch = TRUE
)      #ortalamalar birbirinden farklıdır.

ggAcf(ts_data,lag=216)+
  ggtitle("Otokorelasyon Fonksiyon Grafigi") #bu grafik bize tam olarak mevsimselliğin olduğunu gösteriyor.#güven aralığını aştığından dolayı otokorelasyon olduğunu söyleyebiliriz.Gittikçe şiddeti 
acf_values <-acf(ts_data,plot=FALSE)
acf_values # Yukarıdaki grafiğin sayısal gösterimleri


MannKendall(ts_data) #Trendin varlığını test ediyoruz.Ho red ediliyor yani trend vardır

jarque.bera.test(ts_data)


#Modelimiz
log_data<-log(ts_data)
plot.ts(log_data)

##########SİN ve COS Fonksiyonları ile Trendi ve Mevsimselliği Modellemek#########

tslm(formula=log_data~trend+fourier(ts_data, K=6)) #K değeri L\2 den büyük olamaz.
fit=tslm(formula=log_data~trend+fourier(ts_data, K=6))
summary(fit)

plot(log_data,col="red")
lines(fit$fitted.values)

Acf(fit$residuals)
Anova(fit)
data.res<-resid(fit)
data.res


jarque.bera.test(data.res) #normallik testi normal dağılıyor.

t<-ts(c(1:216))
dwtest(data.res ~ t)
dwtest(fit) # Testte otokorelasyon yoktur çıktısını alıyoruz

accuracy(fit)#ACF değeri 0 a yakın olmasını isteriz.

MSD<-sum((fit$residuals)^2)/length(residuals(fit))
MSD
MSE<-sum((fit$residuals)^2/155)
MSE


######## Lm fonksiyonu ile model ############

fit2 <- lm(log_data ~t+fourier(ts_data,K=6))
summary(fit2)
par(mfrow = c(2, 2))
plot(fit2)
head(fortify(fit2))
accuracy(fit2)


###### YAPAY DEĞİŞKEN ile Trendi ve Mevsimselliği Modellemek ##########

fit3=tslm(log_data~trend+season)
summary(fit3)

plot(log_data,col="red")
lines(fit3$fitted.values)
accuracy(fit3)


Acf(fit3$residuals)
Anova(fit3)

data.res2<-resid(fit3)
data.res2

jarque.bera.test(data.res2) # normal dağılıyor 

t<-ts(c(1:216))
dwtest(data.res2 ~ t) # otokorelasyon gözlemlenmiyor
dwtest(fit3)
accuracy(fit3)

forecast(fit3,h=12)




#Bileşenleri Ayırma 

#Zaman serisi bileşenlere ayırma

decomposed <- stl(log_data, s.window = "periodic", robust = TRUE)
trend <- decomposed$time.series[, "trend"]
seasonal <- decomposed$time.series[, "seasonal"]
remainder <- decomposed$time.series[, "remainder"]

# Bileşenlere ayrılmış verileri görselleştirilmesi
plot(decomposed)


#Hareketli ortalama hesaplama 
moving_avg <- ma(decomposed$x, order = 12) 

moving_avg

#Hareketli ortalama ile veriyi hazırlama
moving_avg_data <- data.frame(log_data, moving_avg) 

moving_avg_data

# Hareketli ortalamanın görselleştirilmesi
ggplot(moving_avg_data, aes(x = log_data, y = moving_avg)) +
  geom_line() +
  labs(x = "Zaman", y = "Hareketli Ortalama") +
  ggtitle("Hareketli Ortalama")





# Basit Üstel Düzleştirme Yöntemi

bud_fit=ets(log_data,model = "ZZZ")
bud_forecast <- forecast(bud_fit, h = 12)
bud_fit # Model çıktıları 
bud_forecast #Tahmin değerlerini gösterir.

bud_fit$par

ggplot() +
  geom_line(aes(x = time(log_data), y = log_data), color = "blue") +
  geom_line(aes(x = time(bud_forecast$mean), y = bud_forecast$mean), color = "red") +
  ggtitle("Basit Üstel Düzleştirme Yöntemi ile Tahminler") +
  xlab("Zaman") +
  ylab("İş Başvurusu")

round(accuracy(bud_forecast,2))
checkresiduals(bud_fit)

# Holt-Winters yöntemi ile tahminlerin hesaplanmas
hw_fit <- HoltWinters(log_data, seasonal = "multiplicative")
hw_forecast <- forecast(hw_fit, h = 12)
hw_fit
hw_forecast
# Tahminlerin görselleştirilmesi
ggplot() +
  geom_line(aes(x = time(log_data), y = log_data), color = "blue") +
  geom_line(aes(x = time(hw_forecast$mean), y = hw_forecast$mean), color = "red") +
  ggtitle("Holt-Winters Yöntemi ile Tahminler") +
  xlab("Zaman") +
  ylab("İş Başvurusu")

holt_fit=holt(log_data,h=12)
holt_fit$model

#Hataların kontrolü
checkresiduals(holt_fit)

#hw komutu ile
x1 <- window(log_data,start=2005)
fit1 <- hw(x1,seasonal="additive")
fit2 <- hw(x1,seasonal="multiplicative")
autoplot(x1) +
  autolayer(fit1, series="HW additive forecasts", PI=FALSE) +
  autolayer(fit2, series="HW multiplicative forecasts",
            PI=FALSE) +
  xlab("Yıl") +
  ylab("İş Başvurusu") +
  ggtitle("Multiplicative ve Additive Tahmin Grafikleri") +
  guides(colour=guide_legend(title="Forecast"))
accuracy(fit1)
accuracy(fit2)

#Hataların kontrolü 
checkresiduals(fit1)

#Hataların kontrolü
checkresiduals(fit2)


# ARIMA MODEL 

# Grafik oluşturma
autoplot(log_data, facets = TRUE) + xlab("Yıllar") + ylab("Aylık Başvuru") #burada veri setimize baktığımızda bir mevsimsellik olduğunu grafiktende anlayabiliyoruz
#artan varyans durumu söz konusu olduğu için data setimize bir log dönüşümü uygalamıştık

#mevsimsellik söz konusu olduğu için mevsimsel fark alıyoruz.
log_data_diff <- diff(log_data, lag = 12) 
ggtsdisplay(log_data_diff, xlab = "Yıl", main = "Mevsimsel Fark")

#2020'ye kadar mevsimsel farklarda aşırı bir yükselme görülmemiştir.2020 yılından sonra mevsimsel farklarda ciddi
#artışlar görülmektedir.
#ACF grafiğine baktığımızda üst ve alt sınırı aşan çizgiler görülmektedir. Bu çizgilerin sayısı bir hayli fazladır. 
#Bu durum verimizde otokorelasyon olduğunu simgeler. Pozitif ve negatif yönde otokorelasyonu etkileyen aşılmış değerler vardır.

#Lag değerleri için belirli otokorelasyon pikleri varsa bu zaman serisinin mevsimvel olduğunu gösterir. 
#ACF grafiğine baktığımızda 12.noktadan sonra serinin yavaş bir şekilde durağanlaştığı görülmektedir.

 

auto.arima(log_data)# bu kod SONUCU EN UYGUN MODELİN ARİMA (1,1,1)(0,0,2) olduğunu görüyoruz
fit<- auto.arima(log_data)

fit2<-Arima(log_data,order = c(1,1,1),seasonal=c(0,1,2))
fit2

fit3<-Arima(log_data,order=c(2,0,3),seasonal = c(2,1,2))
fit3

fit4<-Arima(log_data,order=c(2,1,3),seasonal=c(2,1,2))
fit4

checkresiduals(fit,lag = 36)
log_data %>%
  Arima(order=c(1,1,1),seasonal = c(0,0,2)) %>%
  forecast() %>%
  autoplot() +
  ylab("Başvuru Sayısı")+ xlab("Year")

checkresiduals(fit2,lag=36)
log_data %>%
  Arima(order=c(1,1,1),seasonal = c(0,1,2)) %>%
  forecast() %>%
  autoplot() +
  ylab("Başvuru Sayısı")+ xlab("Year")

checkresiduals(fit3,lag=36)
log_data %>%
  Arima(order=c(2,0,3),seasonal = c(2,1,2)) %>%
  forecast() %>%
  autoplot() +
  ylab("Başvuru Sayısı")+ xlab("Year")

checkresiduals(fit4,lag=36)
log_data %>%
  Arima(order=c(2,1,3),seasonal = c(2,1,2)) %>%
  forecast() %>%
  autoplot() +
  ylab("Başvuru Sayısı")+ xlab("Year")



accuracy(fit4)


