setwd('C:/Users/seokwon/Desktop/study/dacon_LG_forecast')
#################################################################################################################
# Load library
#################################################################################################################
library(data.table)
library(ggplot2)
library(plotly)
library(stringr)
library(lubridate)
library(forcats)
library(dplyr)

#################################################################################################################
# Load Train Data
#################################################################################################################
data_path = 'C:/Users/seokwon/Desktop/study/dacon_LG_forecast/data'
#시스템에 발생한 에러 로그
train_err_data <- fread(file.path(data_path,'train_err_data.csv'))
#시스템 퀄리티 로그
train_quality_data <- fread(file.path(data_path,'train_quality_data.csv'))
#사용자 불만 및 불만이 접수된 시간
train_problem_data <- fread(file.path(data_path,'train_problem_data.csv'))
train_problem_data <- train_problem_data[order(time, user_id)]

#################################################################################################################
# Preprocess Train data
#################################################################################################################
# train_err_data 전처리
#연월일시분초컬럼 추가
train_err_data[,time := parse_date_time(time, orders = c("ymdHMS"))]
train_err_data[,year := year(time)]
train_err_data[,month := month(time)]
train_err_data[,day := day(time)]
train_err_data[,hour := hour(time)]
train_err_data[,min := minute(time)]
# train_err_data[,sec := second(time)]
train_err_data[,ymd_h := ymd_h(paste(year,month,day,hour ,sep = '-'))]
# train_err_data[,ymd_hm := ]

# train_problem_data 전처리
train_problem_data[,time := parse_date_time(time, orders = c("ymdHMS"))]
train_problem_data[,year := year(time)]
train_problem_data[,month := month(time)]
train_problem_data[,day := day(time)]
train_problem_data[,hour := hour(time)]
train_problem_data[,min := minute(time)]
# train_problem_data[,sec := second(time)]
train_problem_data[,ymd_h := ymd_h(paste(year,month,day,hour ,sep = '-'))]

#outlier로 보이는 user_id의 데이터는 제거
sub_user_id <- train_err_data[,.N,by=.(user_id)][N < 10000, user_id]
train_err_sub_data <- train_err_data[user_id %in% sub_user_id]

# problem 여부에 따라 데이터를 나눈 뒤 problem이 있는 user_id는 문제 발생 이전 데이터만 사용
## 전체 problem user_id
problem_user_id <- unique(train_problem_data[,user_id])
train_err_data_with_problem <- train_err_sub_data[user_id %in% problem_user_id]
train_err_data_without_problem <- train_err_sub_data[!user_id %in% problem_user_id]

max_problem_time <- train_problem_data[,.('max_problem_time' = max(time)), by=.(user_id)]
train_err_data_before_problem <- left_join(train_err_data_with_problem, max_problem_time)
#불만 접수한 시간 이전의 데이터만 가져옴
train_err_data_before_problem <- train_err_data_before_problem[time<=max_problem_time]
train_err_data_before_problem[, max_problem_time := NULL]


#problem 있는 데이터와 없는 데이터 합쳐줌
train_err_data_total <- rbind(train_err_data_without_problem, train_err_data_before_problem)

######################################################################################################################

#시간대 별 user_id당 errtype개수 데이터
errtype_by_userid_agg_data <- train_err_data_total[,.(user_id, errtype)]
errtype_by_userid_agg <- dcast(data = errtype_by_userid_agg_data, user_id ~ errtype)

#train_y데이터 생성
train_y <- train_problem_data[,.('user_id' = (unique(user_id)), 'target' = 1)]


train_data <- left_join(errtype_by_userid_agg, train_y)
train_data[is.na(target), target := 0]
train_data <- train_data[order(user_id)]

#train data 저장
fwrite(train_data, 'train_data.csv')
