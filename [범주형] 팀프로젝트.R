## 패키지 설치
install.packages("ggplot2") ; library(ggplot2)
install.packages("dplyr") ; library(dplyr) 
install.packages("car") ; library(car) 
install.packages("PropCIs") ; library(PropCIs) 
install.packages("caret") ; library(caret) 
install.packages("pROC") ; library(pROC)  
install.packages("MASS") ; library(MASS)

#----------------------------------------------------------------------------------------#

## 데이터 불러오기
hyundai = read.csv("C:/Rscript/hyundi.csv")

#----------------------------------------------------------------------------------------#

### 데이터 전처리
hyundai$year = 2020-hyundai$year  # 등록연도 -> 연식

## 가격 이상치 제거
summary(hyundai$price)
hyundai[hyundai$price==max(hyundai$price),] # 주행거리 많은데 가격 높게 설정
hyundai=hyundai[-4248,]
summary(hyundai$price) # max값 변화

## 가격 등급 나누기(기준 : 3사분위)
cutline = quantile(hyundai$price, probs = c(0.75))
hyundai$price_G = ifelse(hyundai$price > cutline, 1, 0)
hyundai$price=NULL
hyundai$price_G=as.factor(hyundai$price_G)

## engineSize 결측
table(hyundai$model,hyundai$engineSize) # engine size 0인 값들 존재(최빈값 대체)
hyundai[hyundai$model==" I10" & hyundai$engineSize==0,8]=1
hyundai[hyundai$model==" I20" & hyundai$engineSize==0,8]=1.2
hyundai[hyundai$model==" I30" & hyundai$engineSize==0,8]=1.6
hyundai[hyundai$model==" I40" & hyundai$engineSize==0,8]=1.7
hyundai[hyundai$model==" I800" & hyundai$engineSize==0,8]=2.5
hyundai[hyundai$model==" IX20" & hyundai$engineSize==0,8]=1.4
hyundai[hyundai$model==" IX35" & hyundai$engineSize==0,8]=1.7
hyundai[hyundai$model==" Tucson" & hyundai$engineSize==0,8]=1.6
table(hyundai$model,hyundai$engineSize) # 최빈값 대체 후

## transmission (Other값 제거)
table(hyundai$model,hyundai$transmission)
hyundai = hyundai %>% filter(transmission != "Other")

## fuelType (Other값 Hybrid로 대체)
table(hyundai$model,hyundai$fuelType)
hyundai[hyundai$fuelType=="Other",5]="Hybrid"

## mpg
summary(hyundai$mpg) # min, max값 특이값 존재 (min값 : 리터당 0.4km / max값 : 리터당 109km)
hyundai[hyundai$mpg==min(hyundai$mpg),] # Ioniq 1.1 특이값
hyundai[hyundai$mpg==max(hyundai$mpg),] # Ioniq 256.8 특이값
hyundai[hyundai$mpg==1.1,7]=78.5 # Ioniq mpg 이상치 최빈값으로 대체
hyundai[hyundai$mpg==256.8,7]=78.5 # Ioniq mpg 이상치 최빈값으로 대체

ggplot(hyundai) +
  geom_histogram(aes(x=mpg,fill=price_G),colour=TRUE) +
  theme_bw()
## 연비가 좋지 않은데 비싼 중고차 -> SUV 가능성 높음 (차량 무게)

## tax
# 오른쪽 끝에 존재하는 최대값 특이값처럼 보임
ggplot(hyundai) +
  geom_boxplot(aes(x=tax), outlier.size=4, outlier.color = "red") +
  theme_bw()

hyundai[hyundai$tax==555,6]=325 # 가장 이상적이라 생각

#----------------------------------------------------------------------------------------#

## SUV 분류
# SUV : Kona, Santa Fe, Tucson, Terracan

hyundai = hyundai %>%
  mutate(SUV = case_when(model == " Santa Fe" ~ "Yes",
                         model == " Terracan" ~ "Yes",
                         model == " Kona" ~ "Yes",
                         model == " Tucson" ~ "Yes",
                         TRUE ~ "No"),
         SUV = factor(SUV, levels = c("No", "Yes")))

suv=xtabs(~SUV + price_G,data=hyundai)
suv=suv[c(2,1),c(2,1)] ; suv

chisq.test(suv) # 독립성 검정결과 귀무가설 기각 = 중고차 등급과 suv여부는 관련있음
chisq.test(suv)$stdres # 표준화잔차

## pi_1 - pi_0
prop.test(c(813,401),c(1061+813,2582+401),correct = FALSE) # wald 신뢰구간
# wald 신뢰구간 0포함 X -> suv이냐 아니냐 두 비율의 차이 존재
diffscoreci(813,1061+813,401,2582+401,conf.level = 0.95) # score 신뢰구간

riskscoreci(813,1061+813,401,2582+401,conf.level = 0.95) 
# suv가 suv가 아닌 그룹보다 1에 속할 확률이 4.49배와 5.44배 사이에 존재

OR = 813*2582/(401*1061) # suv가 그룹1에 속할 오즈는 suv가 아닐 그룹이 그룹1에 속할 오즈보다 4.947배 높다.
se = sqrt(sum(1/suv))
exp(c(log(OR)-1.96*se,log(OR)+1.96*se)) # wald 신뢰구간 (오즈비)
orscoreci(813,813+1061,401,401+2582,conf.level = 0.95) # score 신뢰구간 (오즈비)

#----------------------------------------------------------------------------------------#

## 차량크기에 따른 분류(소형 + 경형 / 중형 + 준중형 / 준대형 + 대형)
# 경차 + 소형 : I10, I20, Ix20, Accent, Getz, Amica, Kona
# 준중형 + 중형 : I30, I40, Ix35, Ioniq, Veloster, Santa Fe, Tucson
# 준대형 + 대형 : Terracan, I800

hyundai = hyundai %>%
  mutate(Size = case_when(model == " I800" ~ "Large",
                          model == " Terracan" ~ "Large",
                         model == " Tucson" ~ "Medium",
                         model == " Santa Fe" ~ "Medium",
                         model == " I40" ~ "Medium",
                         model == " IX35" ~ "Medium",
                         model == " I30" ~ "Medium",
                         model == " Ioniq" ~ "Medium",
                         model == " veloster" ~ "Medium",
                         TRUE ~ "Small"),
         Size = factor(Size, levels = c("Small", "Medium","Large")))

ggplot(hyundai) +
  geom_point(aes(x=Size,y=engineSize), size=5, pch=11, colour="red") +
  theme_bw()

## size의 경우 엔진크기와 연관성이 커서 사용 X
hyundai$Size=NULL

#----------------------------------------------------------------------------------------#

set.seed(20221205)
SL = sample(1:nrow(hyundai),nrow(hyundai)*0.7)
Train = hyundai[SL,]
Test = hyundai[-SL,]

#-------------------------------------------------------------------------------------------#
## 연속형 자료 잔차 이탈도 카이제곱에 수렴 X (다른 방식으로 적합도 검정 필요)
## 상관관계 확인

## 로지스틱 회귀분석
GLM = glm(formula = price_G ~ ., family = binomial(link = "logit"), data = Train)
summary(GLM)
## (1 not defined because of singularities)
## model을 이용해서 SUV를 만들다보니 위와 같은 문제 존재한다고 판단

# 전(SUV 제거)
GLM_b = glm(formula = price_G ~ model + year + transmission + mileage + 
            fuelType + mpg + tax + engineSize , family = binomial(link = "logit"), 
          data = Train)
summary(GLM_b)
vif(GLM_b) # model의 다중공선성문제 발생 -> model 제거

GLM_b = glm(formula = price_G ~  year + transmission + mileage + 
              fuelType + mpg + tax + engineSize , family = binomial(link = "logit"), data = Train)
summary(GLM_b)
vif(GLM_b) # fuelType 제거

GLM_b = glm(formula = price_G ~  year + transmission + mileage + 
               mpg + tax + engineSize , family = binomial(link = "logit"), data = Train)
summary(GLM_b)
vif(GLM_b) # 다중공선성 문제 해결

stepAIC(GLM_b)  # AIC 결과 (mpg 선택 X)
GLM_b = glm(formula = price_G ~ year + transmission + mileage + tax + 
              engineSize, family = binomial(link = "logit"), data = Train)
summary(GLM_b)
Anova(GLM_b)

Predicted = predict(GLM_b, newdata = Test, type = 'response')
Predicted_C = ifelse(Predicted > 0.5, 1, 0)
confusionMatrix(factor(Predicted_C, levels = c(1,0)),
                factor(Test$price_G, levels = c(1,0)))

rocplot=roc(price_G ~ Predicted,Test)
plot.roc(rocplot,legacy.axes = TRUE)
auc(rocplot)

#-------------------------------------------------------------------------------------------#

# 후 (model 제거)
GLM_a = glm(formula = price_G ~ year + transmission + mileage + 
      fuelType + mpg + tax + engineSize + SUV, family = binomial(link = "logit"), 
    data = Train)
summary(GLM_a)
vif(GLM_a) # fuelType 제거

GLM_a = glm(formula = price_G ~ year + transmission + mileage + 
               mpg + tax + engineSize + SUV, family = binomial(link = "logit"), data = Train)
summary(GLM_a)
vif(GLM_a) # 다중공선성 문제 해결

stepAIC(GLM_a) # 모든 변수 선택

Predicted = predict(GLM_a, newdata = Test, type = 'response')
Predicted_C = ifelse(Predicted > 0.5, 1, 0)
confusionMatrix(factor(Predicted_C, levels = c(1,0)),
                factor(Test$price_G, levels = c(1,0)))

rocplot=roc(price_G ~ Predicted,Test)
plot.roc(rocplot,legacy.axes = TRUE)
auc(rocplot)
# 최종 선택 모형 (SUV포함, model 제거)

exp(GLM_a$coefficients) # 회귀계수 해석 : 다른 변수 고정 각 변수 오즈비 증가
