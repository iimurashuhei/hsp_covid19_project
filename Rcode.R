

#（1）データハンドリングと事前分析 ----
library(tidyverse)

#1-1. データの読み込み ----

lowdata <- read_csv("data.csv", na = c(".")) #セルの"999"を欠損値とする
head(lowdata)
names(lowdata)


#1-2. スクリーニングと下位尺度作成 ----

#日本人以外のデータを削除
#28歳以上のデータを削除
#性別無回答のデータを削除

data <- lowdata %>% filter(japanese == "0", age < 28, gender != "回答しない、その他") 
data <- data %>% filter(age != 17) #17歳のデータを削除

data <- data %>% 
  dplyr::mutate(eoe = (hsp1 + hsp2 + hsp4 + hsp6 + hsp9)/5, na.rm = TRUE) %>% #EOEの平均値
  dplyr::mutate(lst = (hsp3 + hsp7 + hsp10)/3, na.rm = TRUE) %>% #LSTの平均値
  dplyr::mutate(aes = (hsp5 + hsp8)/2, na.rm = TRUE) #AESの平均値
names(data)  


#1-3. ヒストグラム ----

#ageの度数分布とヒストグラム
age_count <- dplyr::count(data, age)
knitr::kable(age_count) #テーブル化
ggplot(data = data, mapping = aes(x = age, fill = factor(age))) + geom_bar() #視覚化

#genderの度数分布とヒストグラム
gender_count <- dplyr::count(data, gender)
knitr::kable(gender_count) #テーブル化
ggplot(data = data, mapping = aes(x = gender, fill = factor(gender))) + geom_bar() #視覚化

#hsp1の度数分布とヒストグラム
hsp1_count <- dplyr::count(data, hsp1)
knitr::kable(hsp1_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsp1, fill = factor(hsp1))) + geom_bar() #視覚化

#hsp2の度数分布とヒストグラム
hsp2_count <- dplyr::count(data, hsp2)
knitr::kable(hsp2_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsp2, fill = factor(hsp2))) + geom_bar() #視覚化

#hsp3の度数分布とヒストグラム
hsp3_count <- dplyr::count(data, hsp3)
knitr::kable(hsp3_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsp3, fill = factor(hsp3))) + geom_bar() #視覚化

#hsp4の度数分布とヒストグラム
hsp4_count <- dplyr::count(data, hsp4)
knitr::kable(hsp4_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsp4, fill = factor(hsp4))) + geom_bar() #視覚化

#hsp5の度数分布とヒストグラム
hsp5_count <- dplyr::count(data, hsp5)
knitr::kable(hsp5_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsp5, fill = factor(hsp5))) + geom_bar() #視覚化

#hsp6の度数分布とヒストグラム
hsp6_count <- dplyr::count(data, hsp6)
knitr::kable(hsp6_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsp6, fill = factor(hsp6))) + geom_bar() #視覚化

#hsp7の度数分布とヒストグラム
hsp7_count <- dplyr::count(data, hsp7)
knitr::kable(hsp7_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsp7, fill = factor(hsp7))) + geom_bar() #視覚化

#hsp8の度数分布とヒストグラム
hsp8_count <- dplyr::count(data, hsp8)
knitr::kable(hsp8_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsp8, fill = factor(hsp8))) + geom_bar() #視覚化

#hsp9の度数分布とヒストグラム
hsp9_count <- dplyr::count(data, hsp9)
knitr::kable(hsp9_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsp9, fill = factor(hsp9))) + geom_bar() #視覚化

#brs1の度数分布とヒストグラム
brs1_count <- dplyr::count(data, brs1)
knitr::kable(brs1_count) #テーブル化
ggplot(data = data, mapping = aes(x = brs1, fill = factor(brs1))) + geom_bar() #視覚化

#brs2の度数分布とヒストグラム
brs2_count <- dplyr::count(data, brs2)
knitr::kable(brs2_count) #テーブル化
ggplot(data = data, mapping = aes(x = brs2, fill = factor(brs2))) + geom_bar() #視覚化

#brs3の度数分布とヒストグラム
brs3_count <- dplyr::count(data, brs3)
knitr::kable(brs3_count) #テーブル化
ggplot(data = data, mapping = aes(x = brs3, fill = factor(brs3))) + geom_bar() #視覚化

#brs4の度数分布とヒストグラム
brs4_count <- dplyr::count(data, brs4)
knitr::kable(brs4_count) #テーブル化
ggplot(data = data, mapping = aes(x = brs4, fill = factor(brs4))) + geom_bar() #視覚化

#covid1の度数分布とヒストグラム
covid1_count <- dplyr::count(data, covid1)
knitr::kable(covid1_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid1, fill = factor(covid1))) + geom_bar() #視覚化

#covid2の度数分布とヒストグラム
covid2_count <- dplyr::count(data, covid2)
knitr::kable(covid2_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid2, fill = factor(covid2))) + geom_bar() #視覚化

#covid3の度数分布とヒストグラム
covid3_count <- dplyr::count(data, covid3)
knitr::kable(covid3_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid3, fill = factor(covid3))) + geom_bar() #視覚化

#covid4の度数分布とヒストグラム
covid4_count <- dplyr::count(data, covid4)
knitr::kable(covid4_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid4, fill = factor(covid4))) + geom_bar() #視覚化

#covid5の度数分布とヒストグラム
covid5_count <- dplyr::count(data, covid5)
knitr::kable(covid5_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid5, fill = factor(covid5))) + geom_bar() #視覚化

#covid6の度数分布とヒストグラム
covid6_count <- dplyr::count(data, covid6)
knitr::kable(covid6_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid6, fill = factor(covid6))) + geom_bar() #視覚化

#covid7の度数分布とヒストグラム
covid7_count <- dplyr::count(data, covid7)
knitr::kable(covid7_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid7, fill = factor(covid7))) + geom_bar() #視覚化

#covid8の度数分布とヒストグラム
covid8_count <- dplyr::count(data, covid8)
knitr::kable(covid8_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid8, fill = factor(covid8))) + geom_bar() #視覚化

#covid9の度数分布とヒストグラム
covid9_count <- dplyr::count(data, covid9)
knitr::kable(covid9_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid9, fill = factor(covid9))) + geom_bar() #視覚化

#covid10の度数分布とヒストグラム
covid10_count <- dplyr::count(data, covid10)
knitr::kable(covid10_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid10, fill = factor(covid10))) + geom_bar() #視覚化

#covid11の度数分布とヒストグラム
covid11_count <- dplyr::count(data, covid11)
knitr::kable(covid11_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid11, fill = factor(covid11))) + geom_bar() #視覚化

#covid12の度数分布とヒストグラム
covid12_count <- dplyr::count(data, covid12)
knitr::kable(covid12_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid12, fill = factor(covid12))) + geom_bar() #視覚化

#covid13の度数分布とヒストグラム
covid13_count <- dplyr::count(data, covid13)
knitr::kable(covid13_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid13, fill = factor(covid13))) + geom_bar() #視覚化

#covid14の度数分布とヒストグラム
covid14_count <- dplyr::count(data, covid14)
knitr::kable(covid14_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid14, fill = factor(covid14))) + geom_bar() #視覚化

#covid15の度数分布とヒストグラム
covid15_count <- dplyr::count(data, covid15)
knitr::kable(covid15_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid15, fill = factor(covid15))) + geom_bar() #視覚化

#covid16の度数分布とヒストグラム
covid16_count <- dplyr::count(data, covid16)
knitr::kable(covid16_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid16, fill = factor(covid16))) + geom_bar() #視覚化

#covid17の度数分布とヒストグラム
covid17_count <- dplyr::count(data, covid17)
knitr::kable(covid17_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid17, fill = factor(covid17))) + geom_bar() #視覚化

#covid18の度数分布とヒストグラム
covid18_count <- dplyr::count(data, covid18)
knitr::kable(covid18_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid18, fill = factor(covid18))) + geom_bar() #視覚化

#covid19の度数分布とヒストグラム
covid19_count <- dplyr::count(data, covid19)
knitr::kable(covid19_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid19, fill = factor(covid19))) + geom_bar() #視覚化

#covid20の度数分布とヒストグラム
covid20_count <- dplyr::count(data, covid20)
knitr::kable(covid20_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid20, fill = factor(covid20))) + geom_bar() #視覚化

#covid21の度数分布とヒストグラム
covid21_count <- dplyr::count(data, covid21)
knitr::kable(covid21_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid21, fill = factor(covid21))) + geom_bar() #視覚化

#covid22の度数分布とヒストグラム
covid22_count <- dplyr::count(data, covid22)
knitr::kable(covid22_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid22, fill = factor(covid22))) + geom_bar() #視覚化

#covid23の度数分布とヒストグラム
covid23_count <- dplyr::count(data, covid23)
knitr::kable(covid23_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid23, fill = factor(covid23))) + geom_bar() #視覚化

#covid24の度数分布とヒストグラム
covid24_count <- dplyr::count(data, covid24)
knitr::kable(covid24_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid24, fill = factor(covid24))) + geom_bar() #視覚化

#covid25の度数分布とヒストグラム
covid25_count <- dplyr::count(data, covid25)
knitr::kable(covid25_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid25, fill = factor(covid25))) + geom_bar() #視覚化

#covid26の度数分布とヒストグラム
covid26_count <- dplyr::count(data, covid26)
knitr::kable(covid26_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid26, fill = factor(covid26))) + geom_bar() #視覚化

#covid27の度数分布とヒストグラム
covid27_count <- dplyr::count(data, covid27)
knitr::kable(covid27_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid27, fill = factor(covid27))) + geom_bar() #視覚化

#covid28の度数分布とヒストグラム
covid28_count <- dplyr::count(data, covid28)
knitr::kable(covid28_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid28, fill = factor(covid28))) + geom_bar() #視覚化

#covid29の度数分布とヒストグラム
covid29_count <- dplyr::count(data, covid29)
knitr::kable(covid29_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid29, fill = factor(covid29))) + geom_bar() #視覚化

#covid30の度数分布とヒストグラム
covid30_count <- dplyr::count(data, covid30)
knitr::kable(covid30_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid30, fill = factor(covid30))) + geom_bar() #視覚化

#covid31の度数分布とヒストグラム
covid31_count <- dplyr::count(data, covid31)
knitr::kable(covid31_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid31, fill = factor(covid31))) + geom_bar() #視覚化

#covid32の度数分布とヒストグラム
covid32_count <- dplyr::count(data, covid32)
knitr::kable(covid32_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid32, fill = factor(covid32))) + geom_bar() #視覚化

#covid33の度数分布とヒストグラム
covid33_count <- dplyr::count(data, covid33)
knitr::kable(covid33_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid33, fill = factor(covid33))) + geom_bar() #視覚化

#covid34の度数分布とヒストグラム
covid34_count <- dplyr::count(data, covid34)
knitr::kable(covid34_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid34, fill = factor(covid34))) + geom_bar() #視覚化

#covid35の度数分布とヒストグラム
covid35_count <- dplyr::count(data, covid35)
knitr::kable(covid35_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid35, fill = factor(covid35))) + geom_bar() #視覚化

#covid36の度数分布とヒストグラム
covid36_count <- dplyr::count(data, covid36)
knitr::kable(covid36_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid36, fill = factor(covid36))) + geom_bar() #視覚化

#hsp_meanの度数分布とヒストグラム
hsp_mean_count <- dplyr::count(data, hsp_mean)
knitr::kable(hsp_mean_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsp_mean, fill = factor(hsp_mean))) + geom_histogram(binwidth = 0.5) + guides(fill = "none") #視覚化

#eoeの度数分布とヒストグラム
eoe_mean_count <- dplyr::count(data, eoe)
knitr::kable(eoe_mean_count) #テーブル化
ggplot(data = data, mapping = aes(x = eoe, fill = factor(eoe))) + geom_histogram(binwidth = 0.5) + guides(fill = "none") #視覚化

#lstの度数分布とヒストグラム
lst_mean_count <- dplyr::count(data, lst)
knitr::kable(lst_mean_count) #テーブル化
ggplot(data = data, mapping = aes(x = lst, fill = factor(lst))) + geom_histogram(binwidth = 0.5) + guides(fill = "none") #視覚化

#aesの度数分布とヒストグラム
aes_mean_count <- dplyr::count(data, aes)
knitr::kable(aes_mean_count) #テーブル化
ggplot(data = data, mapping = aes(x = aes, fill = factor(aes))) + geom_histogram(binwidth = 0.5) + guides(fill = "none") #視覚化

#brs_meanの度数分布とヒストグラム
brs_mean_count <- dplyr::count(data, brs_mean)
knitr::kable(brs_mean_count) #テーブル化
ggplot(data = data, mapping = aes(x = brs_mean, fill = factor(brs_mean))) + geom_histogram(binwidth = 0.5) + guides(fill = "none") #視覚化

#covid_meanの度数分布とヒストグラム
covid_mean_count <- dplyr::count(data, covid_mean)
knitr::kable(covid_mean_count) #テーブル化
ggplot(data = data, mapping = aes(x = covid_mean, fill = factor(covid_mean))) + geom_histogram(binwidth = 0.5) + guides(fill = "none") #視覚化


#1-4. 記述統計量 ----

#年齢
data$age <- as.integer(data$age)
mean(data$age) #T1の平均年齢
sd(data$age) #T1のSD

#HSP、レジリエンス、コロナストレス
stat <- data %>% 
  dplyr::summarise(hsp_m = mean(hsp_mean, na.rm = TRUE),
                   hsp_sd = sd(hsp_mean, na.rm = TRUE),
                   eoe_m = mean(eoe, na.rm = TRUE),
                   eoe_sd = sd(eoe, na.rm = TRUE),
                   lst_m = mean(lst, na.rm = TRUE),
                   lst_sd = sd(lst, na.rm = TRUE),
                   aes_m = mean(aes, na.rm = TRUE),
                   aes_sd = sd(aes, na.rm = TRUE),
                   brs_m = mean(brs_mean, na.rm = TRUE),
                   brs_sd = sd(brs_mean, na.rm = TRUE),
                   covid_m = mean(covid_mean, na.rm = TRUE),
                   covid_sd = sd(covid_mean, na.rm = TRUE))
knitr::kable(stat, digits = 2) #出力

##1-5. 内的一貫性の算出 ----
library(psych)
library(GPArotation)

#HSP
omega(data[, c(5:14)],1,fm="ml") #alpha 0.85 #HSP
omega(data[, c(5,6,8,10,13)],1,fm="ml") #alpha 0.79 #EOE
omega(data[, c(7,11,14)],1,fm="ml") #alpha 0.84 #LST
omega(data[, c(9,12)],1,fm="ml") #alpha 0.78 #AES

#BRS
omega(data[, c(15,59,17,60,19,61)],1,fm="ml") #alpha 0.85

#COVID19
omega(data[, c(21:56)],1,fm="ml") #alpha 0.93


##1-6. 相関係数の算出 ----
cor.test(data$hsp_mean, data$brs_mean)#HSP-BRS
cor.test(data$hsp_mean, data$covid_mean)#HSP-COVID
cor.test(data$brs_mean, data$covid_mean)#BRS-COVID

#HSP-BRSプロット
ggplot(data, aes(x = hsp_mean, y = brs_mean, color = gender)) + geom_point() + geom_smooth(method = "lm")
#HSP-COVIDプロット
ggplot(data, aes(x = hsp_mean, y = covid_mean, color = gender)) + geom_point() + geom_smooth(method = "lm")
#BRS-COVIDプロット
ggplot(data, aes(x = brs_mean, y = covid_mean, color = gender)) + geom_point() + geom_smooth(method = "lm")

cor.test(data$eoe, data$brs_mean)#EOE-BRS
cor.test(data$eoe, data$covid_mean)#EOE-COVID
cor.test(data$lst, data$brs_mean)#LST-BRS
cor.test(data$lst, data$covid_mean)#LST-COVID
cor.test(data$aes, data$brs_mean)#AES-BRS
cor.test(data$aes, data$covid_mean)#AES-COVID

cor.test(data$eoe, data$lst)#EOE-LST
cor.test(data$eoe, data$aes)#EOE-AES
cor.test(data$lst, data$aes)#LST-AES


##1-7. 性差 ----

#差の検定
t.test(hsp_mean ~ gender, data = data, var.equal = TRUE) #HSP
t.test(eoe ~ gender, data = data, var.equal = TRUE) #EOE
t.test(lst ~ gender, data = data, var.equal = TRUE) #LST
t.test(aes ~ gender, data = data, var.equal = TRUE) #AES
t.test(brs_mean ~ gender, data = data, var.equal = TRUE) #BRS
t.test(covid_mean ~ gender, data = data, var.equal = TRUE) #COVID

#効果量dの算出
library(effsize)
cohen.d(data$hsp_mean, data$gender)#HSP
cohen.d(data$eoe, data$gender)#EOE
cohen.d(data$lst, data$gender)#LST
cohen.d(data$aes, data$gender)#AES
cohen.d(data$brs_mean, data$gender)#BRS
cohen.d(data$covid_mean, data$gender)#COVID


#（2）階層的重回帰分析 ----


## 2-1. 独立変数の中心化 ----
data_c <- data %>% select("gender","age","hsp_mean","eoe","lst","aes","brs_mean","covid_mean") %>% drop_na()
names(data_c) #確認
data_c$age <- data_c$age - mean(data_c$age) #ageの中心化
data_c$gender <- as.factor(data_c$gender)
data_c$hsp_mean <- data_c$hsp_mean - mean(data_c$hsp_mean) #hsp_meanの中心化
data_c$eoe <- data_c$eoe - mean(data_c$eoe) #eoeの中心化
data_c$lst <- data_c$lst - mean(data_c$lst) #lstの中心化
data_c$aes <- data_c$aes - mean(data_c$aes) #aesの中心化
data_c$brs_mean <- data_c$brs_mean - mean(data_c$brs_mean) #brs_meanの中心化
data_c$covid_mean <- data_c$covid_mean - mean(data_c$covid_mean) #covid_meanの中心化
names(data_c)


## 2-2. HSP ----

##ステップ1：性別、年齢
model_s1 <- lm(data_c$covid_mean ~ data_c$age + data_c$gender, data = data_c)
summary(model_s1)
AIC(model_s1)
BIC(model_s1)

##ステップ2：レジリエンス、HSP
model_s2 <- lm(data_c$covid_mean ~ data_c$age + data_c$gender + data_c$brs_mean + data_c$hsp_mean, data = data_c)
summary(model_s2)
AIC(model_s2)
BIC(model_s2)
anova(model_s1, model_s2) #R^2の増加量の検定

##ステップ3：レジリエンス*HSP交互作用
model_s3 <- lm(data_c$covid_mean ~ data_c$age + data_c$gender + data_c$brs_mean + data_c$hsp_mean + data_c$brs_mean:data_c$hsp_mean, data = data_c)
summary(model_s3)
AIC(model_s3)
BIC(model_s3)
anova(model_s2, model_s3) #R^2の増加量の検定


## 2-3. EOE ----

##ステップ1：性別、年齢
model_s1 <- lm(data_c$covid_mean ~ data_c$age + data_c$gender, data = data_c)
summary(model_s1)
AIC(model_s1)
BIC(model_s1)

##ステップ2：レジリエンス、EOE
model_s2 <- lm(data_c$covid_mean ~ data_c$age + data_c$gender + data_c$brs_mean + data_c$eoe, data = data_c)
summary(model_s2)
AIC(model_s2)
BIC(model_s2)
anova(model_s1, model_s2) #R^2の増加量の検定

##ステップ3：レジリエンス*EOE交互作用
model_s3 <- lm(data_c$covid_mean ~ data_c$age + data_c$gender + data_c$brs_mean + data_c$hsp_mean + data_c$brs_mean:data_c$eoe, data = data_c)
summary(model_s3)
AIC(model_s3)
BIC(model_s3)
anova(model_s2, model_s3) #R^2の増加量の検定


## 2-4. LST ----

##ステップ1：性別、年齢
model_s1 <- lm(data_c$covid_mean ~ data_c$age + data_c$gender, data = data_c)
summary(model_s1)
AIC(model_s1)
BIC(model_s1)

##ステップ2：レジリエンス、LST
model_s2 <- lm(data_c$covid_mean ~ data_c$age + data_c$gender + data_c$brs_mean + data_c$lst, data = data_c)
summary(model_s2)
AIC(model_s2)
BIC(model_s2)
anova(model_s1, model_s2) #R^2の増加量の検定

##ステップ3：レジリエンス*LST交互作用
model_s3 <- lm(data_c$covid_mean ~ data_c$age + data_c$gender + data_c$brs_mean + data_c$hsp_mean + data_c$brs_mean:data_c$lst, data = data_c)
summary(model_s3)
AIC(model_s3)
BIC(model_s3)
anova(model_s2, model_s3) #R^2の増加量の検定


## 2-5. AES ----

##ステップ1：性別、年齢
model_s1 <- lm(data_c$covid_mean ~ data_c$age + data_c$gender, data = data_c)
summary(model_s1)
AIC(model_s1)
BIC(model_s1)

##ステップ2：レジリエンス、AES
model_s2 <- lm(data_c$covid_mean ~ data_c$age + data_c$gender + data_c$brs_mean + data_c$aes, data = data_c)
summary(model_s2)
AIC(model_s2)
BIC(model_s2)
anova(model_s1, model_s2) #R^2の増加量の検定

##ステップ3：レジリエンス*AES交互作用
model_s3 <- lm(data_c$covid_mean ~ data_c$age + data_c$gender + data_c$brs_mean + data_c$hsp_mean + data_c$brs_mean:data_c$aes, data = data_c)
summary(model_s3)
AIC(model_s3)
BIC(model_s3)
anova(model_s2, model_s3) #R^2の増加量の検定


#（3）媒介分析 ----

library(lavaan)

## 3-1. HSP ----
model <- "
      #コントロール
      hsp_mean ~ age + gender
      
      #直接効果
      covid_mean  ~ c*hsp_mean
       
      #媒介
      brs_mean ~ a1*hsp_mean
      covid_mean  ~ b1*brs_mean
 
      #間接効果 (a*b)
      ab := a1*b1
       
      #全体の効果
      total := c + ab
"
fit <- sem(model, data = data, estimator = "ML", se = "bootstrap", bootstrap = 5000)
summary(fit, standardized = TRUE, fit.measure = TRUE, ci = TRUE)
fitMeasures(fit)


## 3-2. EOE ----
model <- "
      #コントロール
      eoe ~ age + gender
      
      #直接効果
      covid_mean  ~ c*eoe
       
      #媒介
      brs_mean ~ a1*eoe
      covid_mean  ~ b1*brs_mean
 
      #間接効果 (a*b)
      ab := a1*b1
       
      #全体の効果
      total := c + ab
"
fit <- sem(model, data = data, estimator = "ML", se = "bootstrap", bootstrap = 5000)
summary(fit, standardized = TRUE, fit.measure = TRUE, ci = TRUE)
fitMeasures(fit)


## 3-3. LST ----
model <- "
      #コントロール
      lst ~ age + gender
      
      #直接効果
      covid_mean  ~ c*lst
       
      #媒介
      brs_mean ~ a1*lst
      covid_mean  ~ b1*brs_mean
 
      #間接効果 (a*b)
      ab := a1*b1
       
      #全体の効果
      total := c + ab
"
fit <- sem(model, data = data, estimator = "ML", se = "bootstrap", bootstrap = 5000)
summary(fit, standardized = TRUE, fit.measure = TRUE, ci = TRUE)
fitMeasures(fit)


## 3-4. AES ----
model <- "
      #コントロール
      aes ~ age + gender
      
      #直接効果
      covid_mean  ~ c*aes
       
      #媒介
      brs_mean ~ a1*aes
      covid_mean  ~ b1*brs_mean
 
      #間接効果 (a*b)
      ab := a1*b1
       
      #全体の効果
      total := c + ab
"
fit <- sem(model, data = data, estimator = "ML", se = "bootstrap", bootstrap = 5000)
summary(fit, standardized = TRUE, fit.measure = TRUE, ci = TRUE)
fitMeasures(fit)


