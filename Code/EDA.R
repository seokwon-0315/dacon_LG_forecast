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


train_problem_data[,time := parse_date_time(time, orders = c("ymdHMS"))]
train_problem_data[,year := year(time)]
train_problem_data[,month := month(time)]
train_problem_data[,day := day(time)]
train_problem_data[,hour := hour(time)]
train_problem_data[,min := minute(time)]
# train_problem_data[,sec := second(time)]
train_problem_data[,ymd_h := ymd_h(paste(year,month,day,hour ,sep = '-'))]


# #train_problem_data의 time칼럼의 마지막 2개 0제거 -> train_err_data와 형식 맞추기 위함
# train_problem_data[,time := as.numeric(substr(time,1,12))]
# 
# #train_problem_data에 problem 컬럼 생성
# train_problem_data[,problem := 1]
# 
# #train_err_data와 train_problem_data join
# tt <- left_join(train_err_data, train_problem_data)
# tt[is.na(problem), problem := 0]
# tt[,.N,by=.(problem)]

#################################################################################################################
# train_err_data EDA
#################################################################################################################
# train_err_data 확인
# -> 약 1650만개 행, 6개 컬럼
head(train_err_data)
str(train_err_data)

#1. user_id : 15000개
length(train_err_data[,unique(user_id)])
#user_id당 데이터 개수 확인 및 Histogram
#outlier로 보이는 데이터 때문에 그래프가 잘 보이지 않음
summary(train_err_data[,.N,by=.(user_id)])
ggplot(train_err_data[,.N,by=.(user_id)], aes(x = N)) + geom_histogram(bins = 100) + 
  labs(title = 'user_id별 데이터 개수 Hist') + theme_bw()

#데이터 개수가 10000개 이하인 user_id만 뽑아도 약 99.2% -> 일단 데이터 개수가 10000개 이하인 얘들만 보는게 나을듯
quantile(train_err_data[,.N,by=.(user_id)][,N], 0.99)

# 데이터 개수가 많은 user_id list 
# -> user_id 24934가 약 22만개 : outlier인가?
top_data_user_id <- train_err_data[,.N,by=.(user_id)][N >= 20000][order(-N)]

#데이터 개수가 많은 user_id 확인
N = 1
tmp_user_id = top_data_user_id[N,user_id]
tmp <- train_err_data[user_id == tmp_user_id, .N, by=.(ymd_h)][order(ymd_h)]
ggplot(tmp, aes(x=ymd_h, y=N)) + geom_line() + theme_bw() + labs(title = str_interp('user_id : ${tmp_user_id}'))
train_err_data[user_id == train_err_data, .N, by=.(errtype)]



#데이터 개수 10000개 이하인 user_id만 선택한 뒤 user_id별 데이터 개수 histogram
sub_user_id <- train_err_data[,.N,by=.(user_id)][N < 10000, user_id]
train_err_sub_data <- train_err_data[user_id %in% sub_user_id]
ggplot(train_err_sub_data[,.N,by=.(user_id)], aes(x = N)) + geom_histogram(bins = 100) + 
  labs(title = 'user_id별 데이터 개수 Hist') + theme_bw()
#user_id당 평균 919개의 데이터 존재
summary(train_err_sub_data[,.N,by=.(user_id)][,N])


#2. time : int -> date로 type변경
#2020년 10월31일 23시59분59초부터 2020년 12월02일 18시51분52초까지의 테이터
train_err_data[,.(min(time),max(time))]


#날짜 - 시간별 데이터 개수
tmp <- train_err_data[,.N,by=.(year,month,day,hour)][order(year,month,day,hour)]
tmp[,ymd := ymd_h(paste(year,month,day,hour ,sep = '-'))]
g <- ggplot(tmp, aes(x=ymd, y=N)) + geom_line(size=0.3) + 
  scale_x_datetime(breaks = '1 days', date_labels = '%d') +
  theme_bw() + labs(title='날짜 - 시간 별 err 개수')
ggplotly(g)
#11월28일 23시 ~ 11월29일 0시에 급격하게 증가함 
tmp[order(-N)]


#3. model_nm : model0~model_8까지 9개 존재
tmp <- train_err_data[,.N,by=.(model_nm)][order(-N)]
ggplot(tmp, aes(x=model_nm, y=N)) + geom_bar(stat='identity') + theme_bw() + labs(title='model_nm별 데이터 개수')


#4. fwver : 뭔지 모르겠음
train_err_data[,.N,by=.(fwver)][order(-N)]


#5. errtype : 41개의 errtype
length(train_err_data[,unique(errtype)])
count_by_errtype <- train_err_data[,.N,by=.(errtype)][order(-N)]
count_by_errtype[,errtype := factor(errtype)]
count_by_errtype[,errtype := fct_reorder(errtype, N)]
ggplot(count_by_errtype, aes(x=errtype, y=N)) + geom_bar(stat='identity') +coord_flip() +theme_bw() +
  labs(title = 'errtype 별 data 개수')

#errttype별 시간별 data 개수 -> 11월29에 errtype 10이 많이 발생 
errtype_time_data_count <- train_err_data[,.N,by=.(errtype, year, month, day, hour)]
errtype_time_data_count[,ymd := ymd_h(paste(year,month,day,hour ,sep = '-'))]
errtype_time_data_count[,errtype := factor(errtype)]
g <- ggplot(errtype_time_data_count, aes(x=ymd, y=N, col=errtype)) + geom_line(size=0.3) + theme_bw() +
    labs(title='errtype별 시간별 데이터 개수')
ggplotly(g)


#errtype별 errcode개수
train_err_data[,.N,by=.(errtype,errcode)][order(-N)]

# 22, 23번에 속한 errcode 종류 및 개수 
# -> 뭔가 connection 관련 에러가 많아보임
train_err_data[errtype %in% c(22,23), .N, by=.(errcode)]
train_err_data[errtype %in% c(23), .N, by=.(errcode)]
train_err_data[errtype %in% c(22), .N, by=.(errcode)]
# 23번이 connection 관련 에러인듯
train_err_data[str_detect(errcode, 'connection'), .N, by=.(errtype)]


#6. errcode : 2803개의 errcode
length(train_err_data[,unique(errcode)])
train_err_data[,.N,by=.(errcode)][order(-N)][1:100]




#################################################################################################################
# train_err_data EDA
#################################################################################################################
#중복데이터가 많음 -> 일단 제거
nrow(unique(train_quality_data))
nrow(train_quality_data)

unique_train_quality_data <- unique(train_quality_data)
unique(unique_train_quality_data[,.(time)][order(time)])




#################################################################################################################
# train_problem_data EDA
#################################################################################################################
#불만이 존재하는 user_id 개수 : 딱 5000개
length(train_problem_data[,unique(user_id)])

#user_id별 불만 개수 : 1~5까지존재
tmp <- train_problem_data[,.N,by=.(user_id)][order(-N)]
table(tmp[,N])

# 날짜-시간 별 불만 접수 개수
# -> 11월 29에 에러 개수가 많았던거에 비해 불만 접수가 많지 않음. 해당 에러가 critical하지 않았나?
tmp <- train_problem_data[,.N,by=.(year,month,day,hour)][order(year,month,day,hour)]
tmp[,ymd := ymd_h(paste(year,month,day,hour ,sep = '-'))]
g <- ggplot(tmp, aes(x=ymd, y=N)) + geom_line() + theme_bw() + labs(title = '날짜 - 시간별 불만 접수 개수')
ggplotly(g)



#################################################################################################################
# 불만 접수한 유저와 접수하지 않은 유저 비교 
#################################################################################################################
# 전체 problem user_id
# problem_user_id <- unique(train_problem_data[,user_id]) 

# problem이 1인 user id
problem_user_id <- train_problem_data[,.N,by=.(user_id)][N==1, user_id]

train_err_data_with_problem <- train_err_data[user_id %in% problem_user_id]
train_err_data_without_problem <- train_err_data[!user_id %in% problem_user_id]

#전체 데이터 중 문제 있는 user에 해당하는 데이터 약 45.8%
nrow(train_err_data_with_problem)/nrow(train_err_data)

max_problem_time <- train_problem_data[,.('max_problem_time' = max(time)), by=.(user_id)]

train_err_data_before_problem <- left_join(train_err_data_with_problem, max_problem_time)
#불만 접수한 시간 이전의 데이터만 가져옴
train_err_data_before_problem <- train_err_data_before_problem[time<=max_problem_time]

# 불만 접수한 유저의 errtype별 개수, 비율
count_by_errtype1 <- train_err_data_before_problem[,.N,by=.(errtype)]
count_by_errtype1[,TOTAL_COUNT := sum(N)]
count_by_errtype1[,PER := round(N/TOTAL_COUNT, 2)]
count_by_errtype1 <- count_by_errtype1[order(-N)]
count_by_errtype1[,errtype := factor(errtype)]
count_by_errtype1[,errtype := fct_reorder(errtype, N)]
ggplot(count_by_errtype1, aes(x=errtype, y=N)) + geom_bar(stat='identity') +coord_flip() +theme_bw() +
  labs(title = 'errtype 별 data 개수')

count_by_errtype1[,problem := 'Y']

# 불만 접수하지 않은 유저의 errtype별 개수, 비율
count_by_errtype2 <- train_err_data_without_problem[,.N,by=.(errtype)]
count_by_errtype2[,TOTAL_COUNT := sum(N)]
count_by_errtype2[,PER := N/TOTAL_COUNT]
count_by_errtype2 <- count_by_errtype2[order(-N)]
count_by_errtype2[,errtype := factor(errtype)]
count_by_errtype2[,errtype := fct_reorder(errtype, N)]
ggplot(count_by_errtype2, aes(x=errtype, y=N)) + geom_bar(stat='identity') +coord_flip() +theme_bw() +
  labs(title = 'errtype 별 data 개수')

count_by_errtype2[,problem := 'N']

# 불만 접수한 유저와 하지 않은 유저 errtpye별 개수, 비율 합쳐서 비교
# -> errtpye 23, 22번이 불만 접수하지 않은 유저보다 많은 것으로 보임
count_by_errtype_total = rbind(count_by_errtype1, count_by_errtype2)
top_errtype <- count_by_errtype_total[,sum(N),by=.(errtype)][order(-V1),errtype][1:20]
ggplot(count_by_errtype_total[errtype %in% top_errtype], aes(x=errtype, y=PER, fill=problem)) +
  geom_bar(stat = 'identity', position = 'dodge') + coord_flip() +theme_bw() + labs(title = '불만 유무에 따른 errtype 비율')


# 불만 접수하기 n일 전부터의 데이터 기준으로 errtype 개수 확인
# a1 : 불만접수된 시간 2일전 ~
train_err_data_before_problem[,tmp_time := max_problem_time - days(1), by=.(user_id)]
tmp_data1 <- train_err_data_before_problem[time >= tmp_time & time <= max_problem_time]
a1 <- tmp_data1[,.N,by=.(errtype)][order(-N)]
a1[,TOTAL_COUNT := sum(N)]
a1[,PER := round(N/TOTAL_COUNT, 2)]
a1[,errtype := factor(errtype)]
a1[,errtype := fct_reorder(errtype, N)]
a1[,type := 'after']

tmp_data2 <- train_err_data_before_problem[time < tmp_time]
a2 <- tmp_data2[,.N,by=.(errtype)][order(-N)]
a2[,TOTAL_COUNT := sum(N)]
a2[,PER := round(N/TOTAL_COUNT, 2)]
a2[,errtype := factor(errtype)]
a2[,errtype := fct_reorder(errtype, N)]
a2[,type := 'before']

aa <- rbind(a1, a2)
ggplot(aa[PER!=0], aes(x=errtype, y=PER, fill=type)) + geom_bar(stat = 'identity', position = 'dodge') +
  coord_flip()
