###################################################
### PLOT MAKER : DATA ANALYSIS USING R (PART 1) ###
###################################################

#######################
### 02 INSTALLING R ###
#######################

3+(4*5)                # 숫자와 산술연산자(+, *)로 구성되어 간단한 계산을 수행하는 명령문
A <- 51:80             # 51-80 사이 자연수를 A에 저장 / [<-]는 오른쪽 내용을 왼쪽에 저장하라는 의미이다.
print(A)               # 저장된 내용을 화면에 출력 / print()와 같은 형식을 [함수]라고 부른다.


# 소스창의 실행버튼을 클릭하면 명령문이 한줄씩 순차적으로 실행된다.
# 잘못된 명령문을 실행하는 경우 error message와 함께 오류가 발생한 이유를 설명한다.


# 주석은 # 기호로 시작하는 명령문을 의미한다. R은 주석을 제외하고 명령문을 실행한다.
# 한 줄 내에서 [#] 이후의 내용은 주석문으로 간주하여 실행되지 않는다.


# Packages는 함수들을 기능별로 묶어놓은 일종의 꾸러미이다.
# 특정 패키지 안의 함수를 이용하기 위해서는 패키지 설치와 불러오기가 선행되어야 한다.

install.packages('ggplot2') # ggplot2 패키지 설치
library(ggplot2) # ggplot2 패키지 불러오기


# help() 괄호 안에 함수를 입력하고 실행하면 함수에 대한 설명을 Help Pane에 출력한다.
help(sort) # sort()라는 함수에 대한 설명 출력


##########################
### 03 DATA TYPES IN R ###
##########################

era <- c(5,4,3,4,5,6)      # c함수를 이용하여 숫자 5,4,3,4,5,6을 하나의 벡터로 연결
year <- 2001:2006          # colon(:) 기호는 단조증가 or 단조감소 형식의 연속된 값 생성 
game <- c(1,2,8,6,4,4)


sum(game)
era*game
era.all <- sum(game*era)/sum(game)
era.all

era[2]                     # era 벡터의 두 번째 성분 추출
era [2:3]                  # era 벡터의 두 번째 성분부터 세 번째 성분까지 추출
cbind(year, game, era)     # cbind()는 같은 길이의 벡터를 열에 넣어 행렬 생성


##########################
### 04 DATAFRAMES IN R ###
##########################

library(tidyverse)

students <- tibble(
  name = c("민수", "영희", "철수", "지수"),
  class = c(1, 1, 2, 2),
  score = c(80, 95, 70, 90)
)

# select() : 이름과 점수 열만 선택
select(students, name, score)

# filter() : 점수가 90점 이상인 학생만 선택
filter(students, score >= 90)

# arrange() : 점수 순으로 학생 정렬
arrange(students, score)              # 점수가 낮은 학생부터 높은 학생 순서로 정렬
arrange(students, desc(score))        # 점수가 높은 학생부터 낮은 학생 순서로 정렬

# mutate() : passed라는 새로운 열 추가(80점 이상이면 TRUE, 아니면 FALSE)
mutate(students, passed = score >= 80)


# %>% 연산 기초 : 80점 이상인 학생 선택 > 점수를 내림차순 정렬 > 이름과 점수만 출력
students %>% filter(score >= 80) %>% arrange(desc(score)) %>% select(name, score)

# 주요 변수에 대한 변수명 변경 : students의 변수명을 각각 A1, A2, A3로 변경
students2 <- students %>% rename(A1 = name, A2 = class, A3 = score)

# 데이터 가공 : A2, A3의 결측치 삭제
df <- students2 %>% select(A1, A2, A3) %>% filter(!is.na(A1), !is.na(A2))

# 데이터 가공 : 그 외 결측치를 0으로 치환 (NA -> 0)
df2 <- df %>% mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))

# 데이터 추가 : df2에 새로운 데이터 A4 = A3 * 0.4 추가
df2$A4 <- (df2$A3)*0.4


#############################
### 05 DATA PREPROCESSING ###
#############################

# 자료 불러오기 & 자료 파악하기
exams <- read.csv("/Users/jaemin/Desktop/RDATA/exams.csv", header = TRUE)
head(exams, 5)

# 기말고사 성적의 중앙값 계산 exams$final은 exams란 데이터셋의 final 열을 의미
median(exams$final)

# 결측치 확인 : is.na() : NA인 경우 T, NA가 아닌 경우 F를 반환하는 함수

# 기말고사 성적의 결측치를 0으로 치환
exams$final[is.na(exams$final)] <- 0
median(exams$final)