rm(list=ls())

set.seed(1)
# 필요한 패키지 로드
library(MASS)

# Gamma-Poisson mixture로 데이터를 생성하는 함수
nb_sim = function(n, mu, phi) {
  # n = sample size
  # phi = dispersion parameter (=1/k)
  # mu = mean parameter
  gi = rgamma(n, shape = 1/phi, scale = mu*phi)
  xi = rpois(n, lambda = gi)
  return(xi)
}

# 수치적 방법으로 `1/k` 추정 함수
estimate_k_numeric = function(data) {
  p0 = mean(data == 0)
  r0 = mean(data)
  
  f = function(phi, p0, r0) {
    return(p0 - (1 + r0 / phi) ^ (-phi))
  }
  
  k = uniroot(f, c(.001, 1000), p0 = p0, r0 = r0)$root
  return(1 / k)
}

# 시뮬레이션 함수
run_simulation = function(n, mu, phi, num_simulations, estimation_method) {
  k_values = numeric(num_simulations)
  
  for (i in 1:num_simulations) {
    data = nb_sim(n, mu, phi)
    if (estimation_method == "numeric") {
      k_values[i] = estimate_k_numeric(data)
    } else if (estimation_method == "mle") {
      d2 <- data.frame(d = data)
      fit <- glm.nb(d ~ 1, data = d2)
      k_values[i] = 1 / fit$theta
    }
  }
  
  mean_k = mean(k_values)
  sd_k = sd(k_values)
  lower_ci <- mean_k - 1.96 * sd_k / sqrt(num_simulations)
  upper_ci <- mean_k + 1.96 * sd_k / sqrt(num_simulations)
  
  return(list(mean_k = mean_k, sd_k = sd_k, lower_ci = lower_ci, upper_ci = upper_ci, k_values = k_values))
}

# 파라미터 설정
n = 300
num_simulations = 100
params = list(
  list(mu = 2, phi = 0.2), # 1/k = 0.2
  list(mu = 2, phi = 2)  # 1/k = 2
)

# 결과 저장
results = list()

# 시뮬레이션 실행
for (param in params) {
  cat("Running simulations for mu =", param$mu, "and phi =", param$phi, "\n")
  
  result_numeric = run_simulation(n, param$mu, param$phi, num_simulations, "numeric")
  result_mle = run_simulation(n, param$mu, param$phi, num_simulations, "mle")
  
  results[[paste("mu", param$mu, "phi", param$phi, "numeric", sep = "_")]] = result_numeric
  results[[paste("mu", param$mu, "phi", param$phi, "mle", sep = "_")]] = result_mle
}

# 결과 출력
for (key in names(results)) {
  cat("\nResults for", key, "\n")
  cat("Mean of 1/k:", results[[key]]$mean_k, "\n")
  cat("Standard Deviation of 1/k:", results[[key]]$sd_k, "\n")
  cat("95% Confidence Interval:", results[[key]]$lower_ci, "-", results[[key]]$upper_ci, "\n")
}





# 모든 히스토그램의 x축 범위 설정
xlim_range = c(0, 4)  # 예시로 설정한 범위, 실제로는 데이터에 맞게 조정 가능



# 히스토그램 그리기
par(mar = c(5, 4, 2, 2) + 0.1)
par(mfrow = c(2, 2))
for (key in names(results)) {
  # key에서 1/k 값을 가져오기 위해 문자열 처리
  key_parts <- strsplit(key, "_")[[1]]
  phi_value <- as.numeric(key_parts[length(key_parts) - 1])  # phi 값 추출
  mu_value <- as.numeric(key_parts[length(key_parts) - 3])   # mu 값 추출
  
  # Numerical Methods 또는 MLE에 따른 히스토그램 제목 설정
  if (grepl("numeric", key)) {
    hist_title <- "Numerical Methods"
  } else if (grepl("mle", key)) {
    hist_title <- "MLE"
  } else {
    hist_title <- ""
  }
  
  hist(results[[key]]$k_values, breaks = 30, xlab = "1/k", xlim = phi_value + c(-1, 1), main = hist_title, cex.main = 0.8)
  
  # Mean of 1/k 파란색 수직선 추가
  abline(v = results[[key]]$mean_k, col = "blue", lwd = 4)
  text(results[[key]]$mean_k, par("usr")[2], round(results[[key]]$mean_k, 3), pos = 2, offset = 1.2, col = "blue", adj = c(0, 0.5), cex=1.5)  # 값 텍스트 추가
  
  # 실제 1/k 값 빨간색 수직선 추가
  abline(v = phi_value, col = "red", lwd = 2)
  text(phi_value, par("usr")[2], round(phi_value, 3), pos = 4, offset = 1, col = "red", adj = c(1, 0.5), cex=1.5)  # 값 텍스트 추가
  
  # 히스토그램 오른쪽 상단에 mu와 phi 값 추가
  mtext(paste("mu =", mu_value), side = 3, line = 1, at = par("usr")[2], col = "black", cex = 0.8)
  mtext(paste("phi =", phi_value), side = 3, line = 0.1, at = par("usr")[2], col = "black", cex = 0.8)
}

'''
Results for mu_2_phi_0.2_numeric 
Mean of 1/k: 0.209988 
Standard Deviation of 1/k: 0.07710599 
95% Confidence Interval: 0.1948753 - 0.2251008 

Results for mu_2_phi_0.2_mle 
Mean of 1/k: 0.1923623 
Standard Deviation of 1/k: 0.05735163 
95% Confidence Interval: 0.1811214 - 0.2036032 

Results for mu_2_phi_2_numeric 
Mean of 1/k: 1.962661 
Standard Deviation of 1/k: 0.2704753 
95% Confidence Interval: 1.909648 - 2.015675 

Results for mu_2_phi_2_mle 
Mean of 1/k: 2.029269 
Standard Deviation of 1/k: 0.2920876 
95% Confidence Interval: 1.972019 - 2.086518 
'''
