rm(list=ls())
# 필요한 패키지 로드
library(MASS)

#sampling하기 위한 함수 선언 
nb_sim = function(n, mu=m, phi=k){
  # n = sample size
  # phi = dispersion parameter (=1/k)
  # mu = mean parameter
  
  gi = rgamma(n, shape = 1/phi, scale = mu*phi)
  xi = rpois(n, lambda = gi)
  return(xi)
}



# 음이항 분포와 포아송 분포를 사용한 피팅 함수
################ 수치적으로 구한 해를 이용함 #################
fit_distributions <- function(d, mu, phi) {
  # 히스토그램
  hist(d, prob = TRUE, col = "gray", right = F, main = paste("Histogram with Fitted Distributions (mu =", mu, ", phi =", phi, ")"))
  
  # x 값 범위 설정
  xvalue <- 0:max(d)
  
  # 음이항 분포 피팅
  r0 <- mean(d)
  p0 <- mean(d == 0)
  f <- function(phi, p0, r0) {
    return(p0 - (1 + r0 / phi) ^ (-phi))
  }
  k <- uniroot(f, c(.001, 1000), p0 = p0, r0 = r0)$root
  
  yvalue_nb <- dnbinom(xvalue, size = k, mu = r0)
  lines(xvalue, yvalue_nb, col = "blue", lwd = 2)
  
  # 포아송 분포 피팅
  yvalue_pois <- dpois(xvalue, lambda = r0)
  lines(xvalue, yvalue_pois, col = "red", lwd = 2)
  
  # 범례 추가
  legend("topright", legend = c("Negative Binomial", "Poisson"), col = c("blue", "red"), lwd = 2)
}

# mu와 phi 값에 대한 조합 생성
mu_values <- c(1, 5)
phi_values <- c(0.1, 5)

# 그래프의 여백 설정
par(mfrow = c(length(mu_values), length(phi_values)), mar = c(3, 3, 1, 1))

# 샘플 데이터 생성 및 피팅
for (mu in mu_values) {
  for (phi in phi_values) {
    d <- nb_sim(n = 300, mu = mu, phi = phi)
    fit_distributions(d, mu = mu, phi = phi)
  }
}

# 다시 원래의 그림 배치로 돌아가기
par(mfrow = c(1, 1))







##############################################################################
#MLE로 구함
# 필요한 패키지 로드
library(MASS)

# 음이항 분포와 포아송 분포를 사용한 피팅 함수 (MLE 이용)
fit_distributions_mle <- function(d, mu, phi) {
  # 음이항 분포 MLE 추정
  fit <- glm.nb(d ~ 1)
  k <- fit$theta
  r0 <- mean(d) 
  
  # 히스토그램
  hist(d, prob = TRUE, col = "gray", right = FALSE, main = paste("Histogram with Fitted Distributions (mu =", mu, ", phi =", phi, ")"))
  
  # x 값 범위 설정
  xvalue <- 0:max(d)
  
  # 음이항 분포 피팅
  yvalue_nb <- dnbinom(xvalue, size = k, mu = r0)
  lines(xvalue, yvalue_nb, col = "blue", lwd = 2)
  
  # 포아송 분포 피팅
  yvalue_pois <- dpois(xvalue, lambda = r0)
  lines(xvalue, yvalue_pois, col = "red", lwd = 2)
  
  # 범례 추가
  legend("topright", legend = c("Negative Binomial", "Poisson"), col = c("blue", "red"), lwd = 2)
}

# mu와 phi 값에 대한 조합 생성
mu_values <- c(1, 5)
phi_values <- c(0.1, 5)

# 그래프의 여백 설정
par(mfrow = c(length(mu_values), length(phi_values)), mar = c(3, 3, 1, 1))

# 샘플 데이터 생성 및 피팅
for (mu in mu_values) {
  for (phi in phi_values) {
    d <- nb_sim(n = 300, mu = mu, phi = phi)
    fit_distributions_mle(d, mu = mu, phi = phi)
  }
}

# 다시 원래의 그림 배치로 돌아가기
par(mfrow = c(1, 1))


d <- nb_sim(n = 300, mu = 1, phi = 0.1)
d2 <- nb_sim(n = 300, mu = 1, phi = 5)


hist(d)
hist(d2)
var(d)# 1.125039
var(d2)# 3.658863
