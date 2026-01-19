# EV RANGE PREDICTION CODES
install.packages("corrplot")
library(corrplot)
if(!require("moments")) { install.packages("moments") }
library(moments)
install.packages("car")
library(car)
install.packages("lmtest")
library(lmtest)
install.packages("sandwich")
library(sandwich)
if(!require("MASS")) { install.packages("MASS") }
library(MASS)

ev <- read.csv(file.choose(), sep=";", stringsAsFactors=FALSE)

str(ev)
head(ev)
summary(ev)
colnames(ev)

# Gerekli S??tunlar
my_data_big <- ev[, c("range_km",               # Y (Hedef)
                      "battery_capacity_kWh",   # X1
                      "top_speed_kmh",          # X2
                      "acceleration_0_100_s",   # X3
                      "torque_nm",              # X4
                      "length_mm",              # X5 (Uzunluk)
                      "width_mm",               # X6 (Geni??lik)
                      "height_mm",              # X7 (Y??kseklik)
                      "towing_capacity_kg",     # X8 (??ekme Kap.-r??mork)
                      "seats",                  # X9 (Koltuk Say??s??)
                      "drivetrain",             # X10 Kategorik-1 (??eki??)
                      "car_body_type",          # X11 Kategorik-2 (Kasa Tipi)
                      "weight_kg")]             # X12 (A????rl??k)

# NA ????lemleri
# towing_capacity_kg yani ??ekme kapasitesindeki bo??luklar s??f??r yani yok demek
my_data_big$towing_capacity_kg[is.na(my_data_big$towing_capacity_kg)] <- 0

# Kategorik De??i??ken D??n??????m?? 
my_data_big$drivetrain    <- as.factor(my_data_big$drivetrain)
my_data_big$car_body_type <- as.factor(my_data_big$car_body_type)
# dummy variable R otomatik yapacak model kurma k??sm??nda.

# NA kontrol?? ve temizleme
sum(is.na(my_data_big))
my_data_big <- na.omit(my_data_big)
sum(is.na(my_data_big))

# Tan??mlay??c?? ??statistikler
summary(my_data_big) 

# Hedef De??i??kenin Da????l??m??
shapiro.test(my_data_big$range_km) # normal da????lm??yor dedi grafik kontrol??nde normal ba??ka test uygula

data_column <- my_data_big$range_km
ks.test(data_column, "pnorm", mean = mean(data_column), sd = sd(data_column)) # normal da????l??yor

# Hedef De??i??kenin Grafik Kontrol??
par(mfrow=c(1,3))
hist(my_data_big$range_km, main="Menzil Histogram??")
boxplot(my_data_big$range_km, 
        horizontal=TRUE, 
        main="Menzil Ayk??r?? De??erler", 
        col="orange")
qqnorm(my_data_big$range_km);qqline(my_data_big$range_km)
par(mfrow=c(1,1))

aykiri_degerler <- boxplot(my_data_big$range_km, plot = FALSE)$out # outlier yok

# S??rekli De??i??kenlerin Histogramlar??
par(mfrow=c(3,4))
hist(my_data_big$range_km, main="Menzil")
hist(my_data_big$battery_capacity_kWh, main="Batarya")
hist(my_data_big$top_speed_kmh, main="H??z")
hist(my_data_big$acceleration_0_100_s, main="H??zlanma")
hist(my_data_big$torque_nm, main="Tork")
hist(my_data_big$length_mm, main="Uzunluk")
hist(my_data_big$width_mm, main="Geni??lik")
hist(my_data_big$height_mm, main="Y??kseklik")
hist(my_data_big$towing_capacity_kg, main="??ekme Kap.")
hist(my_data_big$weight_kg, main="A????rl??k")
par(mfrow=c(1,1))

# S??rekli De??i??kenlerin Boxplotlar??
par(mfrow=c(3,4))
boxplot(my_data_big$range_km, horizontal=TRUE, main="Menzil")
boxplot(my_data_big$battery_capacity_kWh, horizontal=TRUE, main="Batarya")
boxplot(my_data_big$top_speed_kmh, horizontal=TRUE, main="H??z")
boxplot(my_data_big$acceleration_0_100_s, horizontal=TRUE, main="H??zlanma")
boxplot(my_data_big$torque_nm, horizontal=TRUE, main="Tork")
boxplot(my_data_big$length_mm, horizontal=TRUE, main="Uzunluk")
boxplot(my_data_big$width_mm, horizontal=TRUE, main="Geni??lik")
boxplot(my_data_big$height_mm, horizontal=TRUE, main="Y??kseklik")
boxplot(my_data_big$towing_capacity_kg, horizontal=TRUE, main="??ekme Kap.")
boxplot(my_data_big$weight_kg, horizontal=TRUE, main="A????rl??k")
par(mfrow=c(1,1))

# Ba????ms??z De??i??kenlerin Skewnesslar?? 
skewness(my_data_big$battery_capacity_kWh)
skewness(my_data_big$top_speed_kmh)
skewness(my_data_big$acceleration_0_100_s)
skewness(my_data_big$torque_nm)
skewness(my_data_big$length_mm)
skewness(my_data_big$width_mm)
skewness(my_data_big$height_mm)
skewness(my_data_big$towing_capacity_kg)
skewness(my_data_big$seats)
skewness(my_data_big$weight_kg)

# scatter plot
pairs(my_data_big, pch = 19, col='red', lower.panel = NULL)

# Korelasyon Matrisi
numeric_data <- my_data_big[, sapply(my_data_big, is.numeric)]
cor_matrix <- cor(numeric_data)
corrplot(cor_matrix)

# T??m De??i??kenlerin Dahil Oldu??u Model 
model <- lm(range_km ~ ., data = my_data_big)
summary(model) # anlams??z de??i??kenler var 
anova(model)
vif(model)

# hatalar?? standartla??t??rma
standard_residuals <- rstandard(model)
studentized_residuals <- rstudent(model)

# -3 +3 s??n??r??n?? a??anlar
aykiri_indexler1 <- which(abs(standard_residuals) > 3)
aykiri_indexler1
aykiri_indexler2 <- which(abs(studentized_residuals) > 3)
aykiri_indexler2

# temizlenmemi?? verideki 190 195 217 218 459   <-- ??ST SATIR ayk??r?? de??erler -+3 st den sapm????t??r.
nrow(my_data_big) # 471
# E??er ayk??r?? de??er varsa sil, yoksa devam et
if(length(aykiri_indexler1) > 0){
  print(paste("Silinen Sat??rlar:", paste(aykiri_indexler1, collapse=", ")))
  my_data_big <- my_data_big[-aykiri_indexler1, ]
} else {
  print("Ayk??r?? de??er bulunamad??, silme yap??lmad??.")
}
nrow(my_data_big) # 466 oldu

# modeli tekrardan kur (outlierlar ????kt??) TEM??ZLENM???? 
model_n <- lm(range_km ~ ., data = my_data_big)
summary(model_n)
shapiro.test(model_n$residuals)
vif(model_n)

# De??i??ken Se??im Y??ntemleri

# 1-Stepwise Y??ntemi
if(!require("olsrr")) { install.packages("olsrr") }
library(olsrr)
stepwise_sonuc <- ols_step_both_p(model_n, pent = 0.05, prem = 0.1)
stepwise_sonuc

stepwise_model1 <- lm(range_km ~ battery_capacity_kWh + weight_kg + height_mm + 
                        towing_capacity_kg + drivetrain + top_speed_kmh + 
                        car_body_type, data = my_data_big)
summary(stepwise_model1)
vif(stepwise_model1) # ??oklu ba??lant?? sorunu
shapiro.test(stepwise_model1$residuals) # sa??lanmad??
bptest(stepwise_model1) # sa??lanmad??

stepwise_model2 <- lm(range_km ~ battery_capacity_kWh + height_mm + 
                        towing_capacity_kg + drivetrain + top_speed_kmh + 
                        car_body_type, data = my_data_big)
summary(stepwise_model2) 
vif(stepwise_model2) # ??oklu ba??lant?? sorunu biraz 
shapiro.test(stepwise_model2$residuals) # sa??lanmad??
bptest(stepwise_model2) # sa??lanmad??

stepwise_model3 <- lm(range_km ~ battery_capacity_kWh + towing_capacity_kg + 
                        drivetrain + top_speed_kmh + car_body_type, 
                      data = my_data_big)
summary(stepwise_model3)
vif(stepwise_model3) # ??oklu ba??lant?? sorunu yok
shapiro.test(stepwise_model3$residuals) # sa??lanmad??
bptest(stepwise_model3) # sa??lanmad?? 
# STEPWISE MODEL3 umut vaadediyor gerekli d??n??????mlerle d??zelebilir.

# Log D??n??????ml?? Stepwise Model 
log_range <- log(my_data_big$range_km)
stepwise_model_log <- lm(log_range ~ battery_capacity_kWh + towing_capacity_kg + 
                           drivetrain + top_speed_kmh + car_body_type, 
                         data = my_data_big)
summary(stepwise_model_log)
vif(stepwise_model_log)  
shapiro.test(stepwise_model_log$residuals) # normallik sa??lanm??yor
bptest(stepwise_model_log) # sabit varyansl??l??k varsay??m?? sa??lanm??yor

# Karek??k D??n??????ml?? Stepwise Model
my_data_big$sqrt_range <- sqrt(my_data_big$range_km)
stepwise_model_sqrt <- lm(sqrt_range ~ battery_capacity_kWh + towing_capacity_kg + 
                            drivetrain + car_body_type + top_speed_kmh, 
                          data = my_data_big)
vif(stepwise_model_sqrt)
summary(stepwise_model_sqrt) 
shapiro.test(stepwise_model_sqrt$residuals) # normallik varsay??m?? sa??lan??yor
bptest(stepwise_model_sqrt) # sabit varyansl??l??k sa??lan??yor

# A????rl??kl?? En K??????k Kareler (Weighted Least Squares) model 
hatalar <- residuals(stepwise_model3)
agirliklar <- 1 / lm(abs(hatalar) ~ fitted(stepwise_model3))$fitted.values^2
stepwise_model_wls <- lm(range_km ~ battery_capacity_kWh + towing_capacity_kg + 
                           drivetrain + car_body_type + top_speed_kmh, 
                         data = my_data_big, weights = agirliklar)
summary(stepwise_model_wls)
shapiro.test(stepwise_model_wls$residuals) # normallik varsay??m?? sa??lanm??yor
bptest(stepwise_model_wls) # sabit varyansl??l??k sa??land??

stepwise_model_fav <- lm(range_km ~ battery_capacity_kWh + towing_capacity_kg + 
                           drivetrain + car_body_type + torque_nm, 
                         data = my_data_big) # torku modele ald??k top speed ????kt??
vif(stepwise_model_fav) # sorun yok
summary(stepwise_model_fav) 
shapiro.test(stepwise_model_fav$residuals) # p-val=0.659 art??klar normal da????l??yor varsay??m sa??land??
bptest(stepwise_model_fav) # sabit varyansl??l??k varsay??m?? sa??lanmad??

par(mfrow=c(2,2))
plot(stepwise_model_fav)
par(mfrow=c(1,1))

# Model umut vaadediyor d??n??????m uyguluyoruz;

# Log D??n??????ml?? model
log_range <- log(my_data_big$range_km)
model_log <- lm(log_range ~ battery_capacity_kWh + towing_capacity_kg + 
                  drivetrain + car_body_type + torque_nm, 
                data = my_data_big)

vif(model_log)
summary(model_log)
shapiro.test(model_log$residuals) # p-val 0.013 normallik sa??lanmad??
bptest(model_log) # sa??lanmad??

# Karek??k D??n??????ml?? model
my_data_big$sqrt_range <- sqrt(my_data_big$range_km)
model_sqrt <- lm(sqrt_range ~ battery_capacity_kWh + towing_capacity_kg + 
                   drivetrain + car_body_type + torque_nm, 
                 data = my_data_big)
vif(model_sqrt)
summary(model_sqrt) 
shapiro.test(model_sqrt$residuals) # p-val 0.427 normallik varsay??m?? sa??lan??yor
bptest(model_sqrt)  # sa??lanmad?? 

# Box-Cox D??n??????ml?? Model
bc_final <- boxcox(stepwise_model_fav, plotit = TRUE)
lambda_final <- bc_final$x[which.max(bc_final$y)]
my_data_big$range_boxcox_final <- (my_data_big$range_km ^ lambda_final - 1) / lambda_final
model_boxcox_final <- lm(range_boxcox_final ~ battery_capacity_kWh + towing_capacity_kg + 
                           drivetrain + car_body_type + torque_nm, 
                         data = my_data_big)
summary(model_boxcox_final)
shapiro.test(residuals(model_boxcox_final)) # normallik varsay??m?? sa??land??
bptest(model_boxcox_final) # sabit varyansl??l??k sa??lanmad??

# A????rl??kl?? En K??????k Kareler (Weighted Least Squares) model ****
hatalar <- residuals(stepwise_model_fav)
agirliklar <- 1 / lm(abs(hatalar) ~ fitted(stepwise_model_fav))$fitted.values^2
model_wls <- lm(range_km ~ battery_capacity_kWh + towing_capacity_kg + 
                  drivetrain + car_body_type + torque_nm, 
                data = my_data_big, weights = agirliklar)
summary(model_wls) # r2->0.866
vif(model_wls)
shapiro.test(model_wls$residuals) # p-val 0.6468 normallik varsay??m?? sa??lan??yor
bptest(model_wls) # p-val 1 ????kt?? sabit varyansl??l??k sa??land??
# art??klar??n homojenli??i testi:
set.seed(42) 
my_data_big <- my_data_big[sample(nrow(my_data_big)), ] # s??ray?? kar????t??rd??k veri setindeki
durbinWatsonTest(model_wls) # art??klar??n homojenli??i varsay??m?? sa??land??
# N??HA?? MODEL****

# 2-All Possible Best Subsets
numeric_cols <- sapply(my_data_big, is.numeric)
model_sayisal <- lm(range_km ~ ., data = my_data_big[, numeric_cols])
best_subset <- ols_step_best_subset(model_sayisal)
head(best_subset, 10) # cp=p, Radj en y??ksek ve AIC SBIC en d??????k olan 8 de??i??kenli modeldir. 

model_best_subset <- lm(range_km ~ battery_capacity_kWh + weight_kg + length_mm + height_mm +
                          towing_capacity_kg + torque_nm + top_speed_kmh + seats, data = my_data_big)
vif(model_best_subset)
summary(model_best_subset)

# weight vifi ??ok y??ksek modelden at??ld??, length p value su >0.05 anlams??z modelden att??m  
model_best_subset2 <- lm(range_km ~ battery_capacity_kWh + height_mm +
                           towing_capacity_kg + torque_nm + top_speed_kmh + seats, data = my_data_big) 

vif(model_best_subset2) 
summary(model_best_subset2) # lengthte anlams??zd?? o y??zden modelden att??k
anova(model_best_subset2)
shapiro.test(model_best_subset2$residuals) # normallik varsay??m?? sa??lanmad??
bptest(model_best_subset2) # sabit varyansl??l??k sa??lanmad?? 

# varsay??mlar sa??lanmad?? d??n??????m uyguluyorum,

# best subset modeli i??in WLS d??n??????m??
hatalar_best <- abs(residuals(model_best_subset2))
agirlik_modeli <- lm(hatalar_best ~ fitted(model_best_subset2))
agirliklar_best <- 1 / (agirlik_modeli$fitted.values^2)

model_best_wls <- lm(range_km ~ battery_capacity_kWh + height_mm + 
                       towing_capacity_kg + torque_nm + top_speed_kmh + seats,
                     data = my_data_big, weights = agirliklar_best)
summary(model_best_wls)
vif(model_best_wls)
shapiro.test(weighted.residuals(model_best_wls)) # sa??lanmad??
bptest(model_best_wls) # sa??land?? 

# best subset modeli i??in Log D??n??????m??
model_alt_log <- lm(log(range_km) ~ battery_capacity_kWh + height_mm + 
                      towing_capacity_kg + torque_nm + top_speed_kmh + seats,
                    data = my_data_big)
summary(model_alt_log)
shapiro.test(residuals(model_alt_log)) # sa??lanmad??
bptest(model_alt_log) # sa??lanmad??

# best subset2 i??in Box-Cox d??n??????m??
bc_alt <- boxcox(model_best_subset2, plotit = TRUE)
en_iyi_lambda <- bc_alt$x[which.max(bc_alt$y)]
# d??n??????m Form??l??: (y^lambda - 1) / lambda
my_data_big$range_boxcox_alt <- (my_data_big$range_km ^ en_iyi_lambda - 1) / en_iyi_lambda

model_boxcox_alt <- lm(range_boxcox_alt ~ battery_capacity_kWh + height_mm + 
                         towing_capacity_kg + torque_nm + top_speed_kmh + seats,
                       data = my_data_big)
shapiro.test(residuals(model_boxcox_alt)) # sa??lanmad??
bptest(model_boxcox_alt)# sa??lanmad??

# ??ZETLE-> best subsetten anlad??k ki kategorik de??i??kenlerin modeldeki etkisi ??nemli

# 3-Backward Selection Y??ntemi 
backward_sonuc <- ols_step_backward_p(model_n, prem = 0.10)
backward_sonuc
# model kurulmad?? ????nk?? stepwise_model1 ile ayn?? sonu?? 

# 4-Forward Selection Y??ntemi
forward_sonuc <- ols_step_forward_p(model_n, pent = 0.05)
forward_sonuc

model_forward1 <- lm(range_km ~ battery_capacity_kWh + weight_kg + height_mm + towing_capacity_kg 
                     + torque_nm + drivetrain + top_speed_kmh + car_body_type, data = my_data_big)
vif(model_forward1) # ??oklu ba??lant?? sorunu var
summary(model_forward1)
shapiro.test(residuals(model_forward1))# sa??land?? 
bptest(model_forward1)# sa??lanmad??

model_forward2 <- lm(range_km ~ battery_capacity_kWh + height_mm + towing_capacity_kg +
                       torque_nm + drivetrain + top_speed_kmh, data = my_data_big)
vif(model_forward2)
summary(model_forward2)
shapiro.test(model_forward2$residuals) # normal da????lm??yor
bptest(model_forward2)# sa??lanmad??

model_forward3 <- lm(range_km ~ battery_capacity_kWh + height_mm + towing_capacity_kg 
                     + drivetrain + top_speed_kmh + car_body_type, data = my_data_big)
vif(model_forward3)
summary(model_forward3)
shapiro.test(model_forward3$residuals) # normal da????lm??yor
bptest(model_forward3) # sa??lanmad??

# Ayr?? y??ntemlerden ve manuel yapt??klar??m??z aras??ndan en uygun "alternatif 3 model" se??ildi
# Aday 1: Backward / Stepwise (Tork YOK)
model_aday1 <- stepwise_model1 

# Aday 2: Forward (Tork VAR ama VIF y??ksek)
model_aday2 <- model_forward1

# Aday 3: Manuel Se??im (Bizim Favorimiz - Tork VAR, VIF Temiz)
model_aday3 <- model_wls

# 2. Metrikleri Hesaplayan "Ak??ll??" Fonksiyon
# --------------------------------------------------------------------
ozet_cikar_pro <- function(model) {
  s <- summary(model)
  
  # A) RMSE (E??itim Hatas??) Hesab??
  rmse_val <- sqrt(mean(residuals(model)^2))
  
  # B) Varsay??m Testleri (Hata verirse NA d??ns??n)
  p_shapiro <- tryCatch(shapiro.test(residuals(model))$p.value, error=function(e) NA)
  p_bptest  <- tryCatch(bptest(model)$p.value, error=function(e) NA)
  
  # C) VIF Hesab?? (Kategorik De??i??ken ??nlemi!)
  vif_vals <- tryCatch(vif(model), error=function(e) NA)
  max_vif <- NA
  
  if (!all(is.na(vif_vals))) {
    if (is.matrix(vif_vals)) {
      # E??er kategorik de??i??ken varsa VIF matris d??ner (GVIF).
      # Standart VIF ile k??yaslamak i??in son s??tunun karesini al??r??z.
      # (GVIF^(1/(2*Df)))^2 yakla????k olarak VIF'e denktir.
      max_vif <- max(vif_vals[, ncol(vif_vals)]^2)
    } else {
      # Sadece say??sal de??i??ken varsa direkt vekt??r d??ner
      max_vif <- max(vif_vals)
    }
  }
  
  # ????kt?? Vekt??r??
  c(
    "Adj. R-Squared"      = round(s$adj.r.squared, 4),
    "RMSE (Hata)"         = round(rmse_val, 2),
    "Normallik (Shapiro)" = round(p_shapiro, 3),
    "Varyans (BP Test)"   = round(p_bptest, 3),
    "Max VIF De??eri"      = round(max_vif, 1)
  )
}

# 3. CBIND ile Yan Yana Koyma (B??y??k Kar????la??t??rma)
karsilastirma_tablosu <- cbind(
  " Stepwise/Backward " = ozet_cikar_pro(model_aday1),
  "  Forward "    = ozet_cikar_pro(model_aday2),
  "  Manuel "  = ozet_cikar_pro(model_aday3)
)

print("--- ALTERNAT??F MODELLER??N KAR??ILA??TIRMASI ---")
print(karsilastirma_tablosu) 

# model wls ile devam etmeye karar verdik

residualPlots(model_wls, 
              type = "rstudent", # Studentized art??klar (daha hassas)
              layout = c(2,3),   # Grafikleri 2 sat??r 3 s??tun diz
              main = "Ba????ms??z De??i??kenler vs. Art??klar") 
# e??risel g??z??ken var batarya kapasitesi tork ve towing capacity!!! 

# Polinom Model (Kareli Terim Eklenmi??)
model_poly <- lm(range_km ~ battery_capacity_kWh + I(battery_capacity_kWh^2) + 
                   towing_capacity_kg + drivetrain + car_body_type + torque_nm, 
                 data = my_data_big, 
                 weights = agirliklar)

summary(model_poly)
anova(model_wls, model_poly)
dev.off()
residualPlots(model_poly, 
              type = "rstudent", # Studentized art??klar (daha hassas)
              layout = c(3,3),   # Grafikleri 2 sat??r 3 s??tun diz
              main = "Ba????ms??z De??i??kenler vs. Art??klar") 

model_poly_final <- lm(range_km ~ battery_capacity_kWh + I(battery_capacity_kWh^2) + 
                         towing_capacity_kg + I(towing_capacity_kg^2) + 
                         torque_nm + I(torque_nm^2) + 
                         drivetrain + car_body_type, 
                       data = my_data_big, 
                       weights = agirliklar) # e??risellik giderme

summary(model_poly_final)
shapiro.test(residuals(model_poly_final)) # ba??ar??l??
bptest(model_poly_final) # ba??ar??l?? 
durbinWatsonTest(model_poly_final) # ba??ar??l??
residualPlots(model_poly_final, 
              type = "rstudent", # Studentized art??klar (daha hassas)
              layout = c(3,3),   # Grafikleri 2 sat??r 3 s??tun diz
              main = "Ba????ms??z De??i??kenler vs. Art??klar") 

# normallik varsay??m?? grafi??i
std_hatalar <- rstudent(model_poly_final)
par(mfrow=c(1,2)) 
qqnorm(std_hatalar, 
       main = "Normal Q-Q Plot", 
       xlab = "Teorik Kantiller", ylab = "Standartla??t??r??lm???? Art??klar",
       col = "black", pch = 19) 
qqline(std_hatalar, col = "red", lwd = 2) 
hist(std_hatalar, 
     freq = FALSE, 
     main = "Hatalar??n Da????l??m?? (Histogram)", 
     xlab = "Art??klar", 
     col = "cyan")
par(mfrow=c(1,1))

# Train Test ile overfitting kontrol?? yapt??k
set.seed(123)
train_index <- sample(1:nrow(my_data_big), 0.8 * nrow(my_data_big))
train_data <- my_data_big[train_index, ]
test_data  <- my_data_big[-train_index, ]

model_basit <- lm(range_km ~ battery_capacity_kWh + I(battery_capacity_kWh^2) + 
                    towing_capacity_kg + torque_nm + drivetrain + car_body_type, 
                  data = train_data)

model_karmasik <- lm(range_km ~ battery_capacity_kWh + I(battery_capacity_kWh^2) + 
                       towing_capacity_kg + I(towing_capacity_kg^2) + 
                       torque_nm + I(torque_nm^2) + 
                       drivetrain + car_body_type, 
                     data = train_data)

pred_basit <- predict(model_basit, test_data)
pred_karmasik <- predict(model_karmasik, test_data)

rmse_basit <- sqrt(mean((test_data$range_km - pred_basit)^2))
rmse_karmasik <- sqrt(mean((test_data$range_km - pred_karmasik)^2))

print(paste("Basit Model Hata (RMSE):", round(rmse_basit, 2)))
print(paste("Karma????k Model Hata (RMSE):", round(rmse_karmasik, 2))) # overfitting yapm??yor

#  LEVERAGE VE ETK??N G??ZLEM ANAL??Z?? 
leverage_degerleri <- hatvalues(model_poly_final)
n <- nobs(model_poly_final)
p <- length(coef(model_poly_final))

# Kritik E??ik De??eri Hesab?? (2*p / n)
kritik_h <- (2 * p) / n 
print(paste("Kritik Kald??ra?? De??eri:", round(kritik_h, 4)))

par(mfrow=c(1,1))
plot(leverage_degerleri,
     type = "h",
     ylab = "Leverage (hii)",
     xlab = "G??zlem Index",
     main = "model_poly_final - Kald??ra?? De??erleri")

abline(h = kritik_h, col = "red", lty = 2)
text(x = 1:n, 
     y = leverage_degerleri, 
     labels = ifelse(leverage_degerleri > kritik_h, names(leverage_degerleri), ""), 
     pos = 3, cex = 0.7, col = "red")

# Y??ksek Kald??ra??l?? G??zlemlerin Tespiti
yuksek_kaldirac_index <- which(leverage_degerleri > kritik_h)

# Analiz Tablosu (Leverage vs Rstudent)
analiz_tablosu <- data.frame(
  Gozlem_No = yuksek_kaldirac_index,
  Arac_Modeli = row.names(my_data_big)[yuksek_kaldirac_index],
  Kaldirac_Degeri = round(leverage_degerleri[yuksek_kaldirac_index], 3),
  Hata_Rstudent = round(rstudent(model_poly_final)[yuksek_kaldirac_index], 3),
  Durum = ifelse(abs(rstudent(model_poly_final)[yuksek_kaldirac_index]) > 3, "K??T?? (Bozucu)", "??Y?? (G????lendirici)")
)
print("--- Y??KSEK KALDIRA??LI G??ZLEMLER TABLOSU ---")
print(analiz_tablosu)

standardized.resid.poly <- model_poly_final$residuals / sqrt(var(model_poly_final$residuals))
plot(model_poly_final$fitted.values, standardized.resid.poly,
     main = "Art??klar vs Tahmin De??erleri",
     xlab = "Tahmin (Fitted)", ylab = "Standartla??t??r??lm???? Art??klar")
abline(h=0, col="red", lty=2)

# COOK'S DISTANCE 

# 1. Cook Mesafelerini Hesapla
cooks_final <- cooks.distance(model_poly_final)

# 2. Kritik E??ik De??erini Belirle (4/n kural??)
kritik_cook_final <- 4 / n
cat("Kritik Cook E??ik De??eri (4/n):", kritik_cook_final, "\n")

# 3. Grafik ??izimi 
plot(cooks_final, 
     type = "h",  
     main = "model_poly_final: Cook's Distance Analizi", 
     ylab = "Cook's Distance (Etki Miktar??)",
     xlab = "G??zlem Numaras?? (Index)",
     col = "darkgray")

# Kritik e??ik ??izgisini ekledik 
abline(h = kritik_cook_final, col = "red", lty = 2, lwd = 2)

text(x = 1:n, 
     y = cooks_final, 
     labels = ifelse(cooks_final > kritik_cook_final, names(cooks_final), ""), 
     pos = 3,       
     cex = 0.7,    
     col = "blue")
etkin_gozlemler_final <- which(cooks_final > kritik_cook_final)

if(length(etkin_gozlemler_final) > 0){
  print("Dikkat! A??a????daki g??zlemler e??ik de??erini a??t?? (Etkin G??zlemler):")
  print(etkin_gozlemler_final)
} else {
  print("Tebrikler! E??ik de??eri a??an (modeli bozan) etkin g??zlem bulunamad??.")
}
dev.off()

# --- NORMALL??K KONTROLLER?? ---
par(mfrow=c(2,2))
plot(model_poly_final)
qqnorm(standardized.resid.poly, main="Normal Q-Q Plot"); qqline(standardized.resid.poly, col="red")
hist(model_poly_final$residuals, main="Art??klar??n Histogram??", xlab="Art??klar", col="lightblue")
par(mfrow=c(1,1))

# Etki Grafi??i (Influence Plot - car paketi gerekir)
if(require("car")) {
  influencePlot(model_poly_final, 
                main="Etki Grafi??i (Influence Plot)",
                sub="Daire b??y??kl?????? Cook Uzakl??????n?? g??sterir")
}

# G??ven Aral??klar??
guven_araliklari <- confint(model_poly_final, level = 0.95)
guven_araliklari

# TOGG T10F yeni g??zlem i??in; katalogda 623 yaz??yor BA??ARILI
togg_t10f <- data.frame(
  battery_capacity_kWh = 88.5,
  towing_capacity_kg   = 0,      
  drivetrain           = "RWD",  # Arkadan ??ti?? (Rear Wheel Drive)
  car_body_type        = "Sedan",  # Kasa Tipi
  torque_nm            = 350    # 160 kW motorun torku
)

togg_guven <- confint(model_poly_final, newdata = togg_t10f, interval = "confidence", level = 0.95)
togg_guven

togg_predict <- predict(model_poly_final, newdata = togg_t10f, interval = "confidence", level = 0.95)
togg_predict