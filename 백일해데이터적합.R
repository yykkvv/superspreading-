


result # high/middle/female/male 분류된 백일해 데이터 

library(readxl)
library(igraph)
library(tidyverse)
library(MASS)

# 음이항 분포 및 AIC, BIC 계산 함수
fit_and_plot <- function(data, category) {
  fit <- glm.nb(data ~ 1)
  k <- fit$theta
  r0 <- mean(data)
  phi <- 1 / k
  N <- length(data)
  
  # 제목 생성
  title <- sprintf("%s (phi=%.3f)", category, phi)
  
  # 히스토그램
  hist(data, prob = TRUE, col = "gray", main = title, xlab = "Number of Offspring", ylab = "Density", right = FALSE)
  
  # x 값 범위 설정
  xvalue <- 0:max(data)
  
  # 음이항 분포 피팅
  yvalue_nb <- dnbinom(xvalue, size = k, mu = r0)
  lines(xvalue, yvalue_nb, col = "blue", lwd = 2)
  
  # 포아송 분포 피팅
  yvalue_pois <- dpois(xvalue, lambda = r0)
  lines(xvalue, yvalue_pois, col = "red", lwd = 2)
  
  # 범례 추가
  legend("topright", legend = c("Negative Binomial", "Poisson"), col = c("blue", "red"), lwd = 2)
  
  # AIC, BIC 계산
  AIC_nb <- AIC(fit)
  BIC_nb <- BIC(fit)
  
  fit_pois <- glm(data ~ 1, family = poisson)
  AIC_pois <- AIC(fit_pois)
  BIC_pois <- BIC(fit_pois)
  
  return(list(AIC_nb = AIC_nb, BIC_nb = BIC_nb, AIC_pois = AIC_pois, BIC_pois = BIC_pois))
}

# 전체 데이터에 대해 히스토그램 및 AIC, BIC 계산
par(mfrow = c(1, 1))
overall_results <- fit_and_plot(result$overall, "Overall data")

# high, middle, female, male 데이터에 대해 히스토그램 및 AIC, BIC 계산
par(mfrow = c(2, 2))
high_results <- fit_and_plot(result$high, "High School")
middle_results <- fit_and_plot(result$middle, "Middle School")
female_results <- fit_and_plot(result$female, "Female")
male_results <- fit_and_plot(result$male, "Male")

# 결과 출력
cat("\nOverall:\n")
print(overall_results)

cat("\nHigh School:\n")
print(high_results)

cat("\nMiddle School:\n")
print(middle_results)

cat("\nFemale:\n")
print(female_results)

cat("\nMale:\n")
print(male_results)



'''
Overall:
> print(overall_results)
$AIC_nb
[1] 126.364

$BIC_nb
[1] 130.2277

$AIC_pois
[1] 194.7135

$BIC_pois
[1] 196.6453

> cat("\nHigh School:\n")

High School:
> print(high_results)
$AIC_nb
[1] 86.85857

$BIC_nb
[1] 89.96927

$AIC_pois
[1] 117.0469

$BIC_pois
[1] 118.6023

> cat("\nMiddle School:\n")

Middle School:
> print(middle_results)
$AIC_nb
[1] 55.33793

$BIC_nb
[1] 57.32939

$AIC_pois
[1] 68.26615

$BIC_pois
[1] 69.26188

> cat("\nFemale:\n")

Female:
> print(female_results)
$AIC_nb
[1] 63.32412

$BIC_nb
[1] 65.84031

$AIC_pois
[1] 62.56401

$BIC_pois
[1] 63.82211

> cat("\nMale:\n")

Male:
> print(male_results)
$AIC_nb
[1] 84.20544

$BIC_nb
[1] 87.19846

$AIC_pois
[1] 118.9259

$BIC_pois
[1] 120.4224
'''
