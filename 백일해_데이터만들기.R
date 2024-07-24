
rm(list=ls())

library(readxl)
library(igraph)
library(tidyverse)

############################전체데이터만들기(offspring_values)################
linelist <- read_excel("부산체중고_백일해(52명).xlsx", sheet = 1)

#id변수 만들기
linelist <- linelist %>% mutate(id = row_number())

#원래 데이터에서 특정 변수만 추출 
cl_linelist <- linelist %>% select(id, "KEY(이름+주민번호)","환자성별", "Source of infection ID", "cluster characteristric")

#변수명 바꾸기
cl_linelist <- cl_linelist %>% rename(SOI = "Source of infection ID", KEY = "KEY(이름+주민번호)", cluster = "cluster characteristric" , sex = "환자성별")


##key와 SOI를 숫자로 만들기 
# KEY와 id의 매핑 테이블 생성
mapping_table <- cl_linelist %>%
  select(KEY, id) %>%
  rename(SOI = KEY, SOI_id = id)

# SOI 값의 id로 변환
cl_linelist <- cl_linelist %>%
  left_join(mapping_table, by = c("SOI" = "SOI")) %>%
  mutate(SOI_num = ifelse(SOI == "모름", NA, SOI_id)) %>%
  select(-SOI_id, -KEY, -SOI) %>% 
  rename(SOI = SOI_num)# 기존 SOI_id 변수는 제거

#SOI가 NA인 행 지우기 (일단 SOI모르는 경우도 포함시켜봄봄)
cl_linelist$SOI %>% is.na() %>% sum() #4개의 NA가 포함 



#NA값 제거 
cl_linelist <- cl_linelist %>% filter(!is.na(SOI))
nrow(cl_linelist) #48개의 데이터 







#####1.5 building the network #####

# id: infectee,  SOI: infector


total_cl_linelist <- cl_linelist %>% select(id, SOI)



prior_graph = graph_from_data_frame(total_cl_linelist %>%
                                      mutate(SOI_n = as.numeric(SOI)) %>% #SOI를 숫자형으로 바꿈 
                                      select(SOI_n, id) %>%
                                      dplyr::rename(.data = ., from = SOI_n, to = id) %>% #변수이름 바꾸기 : SOI_n변수를 from으로, id변수를to로 
                                      mutate(name = from), #from변수(SOI_n변수)를 name이라는 새로운 변수에 저장 
                                    direct = TRUE) #방향성 그래프 생성 

# # NA를 포함시키면 다음과 같은 오류가 뜸 
# '''
# 경고메시지(들):
# graph_from_data_frame(total_cl_linelist %>% mutate(SOI_n = as.numeric(SOI)) %>% 에서:
#   In `d' `NA' elements were replaced with string "NA"
# '''

print(length(V(prior_graph)))  # 그래프에 포함된 노드 수 확인

prior_graph <- igraph::simplify(prior_graph, remove.multiple = TRUE, remove.loops = TRUE)

library(dplyr)
library(igraph)



# 전체 네트워크에서 각 노드의 자식 노드 수를 계산합니다.
offspring_dist <- degree(prior_graph, mode = "out")

# 결과를 데이터프레임으로 변환
dtf_off <- data.frame(id = names(offspring_dist), offspring_count = as.numeric(offspring_dist))

# 자식 노드 수를 포함하는 값만 추출 (0 포함)
offspring_values <- dtf_off$offspring_count

# 결과 출력
offspring_values

length(offspring_values) #51개 

############################분류데이터 만들기(result)###################


# 네트워크 생성 및 자식 노드 수 계산 함수
calculate_offspring_counts <- function(data) {
  graph <- graph_from_data_frame(data %>%
                                   mutate(SOI_n = as.numeric(SOI)) %>%
                                   select(SOI_n, id) %>%
                                   rename(from = SOI_n, to = id),
                                 directed = TRUE)
  
  prior_graph <- igraph::simplify(graph, remove.multiple = TRUE, remove.loops = TRUE)
  offspring_dist <- degree(prior_graph, mode = "out")
  return(as.numeric(offspring_dist))
}



# 클러스터 및 성별별 자식 노드 수 계산
result <- list(
  overall = calculate_offspring_counts(cl_linelist %>% select(-cluster, -sex)),
  high = calculate_offspring_counts(cl_linelist %>% filter(cluster == "고등학교") %>% select(-cluster, -sex)),
  middle = calculate_offspring_counts(cl_linelist %>% filter(cluster == "중학교") %>% select(-cluster, -sex)),
  female = calculate_offspring_counts(cl_linelist %>% filter(sex == "여") %>% select(-cluster, -sex)),
  male = calculate_offspring_counts(cl_linelist %>% filter(sex == "남") %>% select(-cluster, -sex))
)



# 결과 출력
result

length(result$overall)
length(result$high)
length(result$middle)
length(result$female)
length(result$male)







##############################합친거##################################


rm(list=ls())

library(readxl)
library(igraph)
library(tidyverse)

############################전체데이터만들기(offspring_values)################
linelist <- read_excel("부산체중고_백일해(52명).xlsx", sheet = 1)

#id변수 만들기
linelist <- linelist %>% mutate(id = row_number())

#원래 데이터에서 특정 변수만 추출 
cl_linelist <- linelist %>% select(id, "KEY(이름+주민번호)","환자성별", "Source of infection ID", "cluster characteristric")

#변수명 바꾸기
cl_linelist <- cl_linelist %>% rename(SOI = "Source of infection ID", KEY = "KEY(이름+주민번호)", cluster = "cluster characteristric" , sex = "환자성별")


##key와 SOI를 숫자로 만들기 
# KEY와 id의 매핑 테이블 생성
mapping_table <- cl_linelist %>%
  select(KEY, id) %>%
  rename(SOI = KEY, SOI_id = id)

# SOI 값의 id로 변환
cl_linelist <- cl_linelist %>%
  left_join(mapping_table, by = c("SOI" = "SOI")) %>%
  mutate(SOI_num = ifelse(SOI == "모름", NA, SOI_id)) %>%
  select(-SOI_id, -KEY, -SOI) %>% 
  rename(SOI = SOI_num)# 기존 SOI_id 변수는 제거

#SOI가 NA인 행 지우기 (일단 SOI모르는 경우도 포함시켜봄봄)
cl_linelist$SOI %>% is.na() %>% sum() #4개의 NA가 포함 



#NA값 제거 
cl_linelist <- cl_linelist %>% filter(!is.na(SOI))
nrow(cl_linelist) #총48개의 데이터 




###########################################
# 네트워크 생성 및 자식 노드 수 계산 함수
calculate_offspring_counts <- function(data) {
  graph <- graph_from_data_frame(data %>%
                                   mutate(SOI_n = as.numeric(SOI)) %>%
                                   select(SOI_n, id) %>%
                                   rename(from = SOI_n, to = id),
                                 directed = TRUE)
  
  prior_graph <- igraph::simplify(graph, remove.multiple = TRUE, remove.loops = TRUE)
  offspring_dist <- degree(prior_graph, mode = "out")
  return(as.numeric(offspring_dist))
}



# 클러스터 및 성별별 자식 노드 수 계산
result <- list(
  overall = calculate_offspring_counts(cl_linelist %>% select(-cluster, -sex)),
  high = calculate_offspring_counts(cl_linelist %>% filter(cluster == "고등학교") %>% select(-cluster, -sex)),
  middle = calculate_offspring_counts(cl_linelist %>% filter(cluster == "중학교") %>% select(-cluster, -sex)),
  female = calculate_offspring_counts(cl_linelist %>% filter(sex == "여") %>% select(-cluster, -sex)),
  male = calculate_offspring_counts(cl_linelist %>% filter(sex == "남") %>% select(-cluster, -sex))
)


cl_linelist %>% nrow() #48개
cl_linelist %>% filter(cluster == "고등학교") %>% nrow() #29개 
cl_linelist %>% filter(cluster == "중학교") %>% nrow() #19개 
cl_linelist %>% filter(sex == "여") %>% nrow() #18개
cl_linelist %>% filter(sex == "남") %>% nrow() #30개 

# 결과 출력
result

length(result$overall) #51
length(result$high) # 35
length(result$middle) #20 
length(result$female) #26
length(result$male) #33
