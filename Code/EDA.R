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


# train_quality_data 전처리
#중복데이터가 많음 -> 일단 제거
nrow(unique(train_quality_data))
nrow(train_quality_data)
train_quality_data <- unique(train_quality_data)

str(train_quality_data)
train_quality_data[,time := parse_date_time(time, orders = c("ymdHMS"))]
# quality5,7,8,9,10이 string type
# -> 진짜 string type이 아닌 숫자에 ',' 때문에 string
# -> 제거하고 int로 바꿔줌
train_quality_data[,quality_5 := as.integer(str_remove_all(quality_5, ','))]
train_quality_data[,quality_7 := as.integer(str_remove_all(quality_7, ','))] 
train_quality_data[,quality_8 := as.integer(str_remove_all(quality_8, ','))] 
train_quality_data[,quality_9 := as.integer(str_remove_all(quality_9, ','))] 
train_quality_data[,quality_10 := as.integer(str_remove_all(quality_10, ','))] 
# quality0,2이 numeric type -> int type
train_quality_data[,quality_0 := as.integer(quality_0)]
train_quality_data[,quality_2 := as.integer(quality_2)]



#시간대 별 user_id당 errtype개수 데이터
errtype_by_userid_agg_data <- train_err_data[,.(user_id, year, errtype)]
errtype_by_userid_agg <- dcast(data = errtype_by_userid_agg_data, user_id + year ~ errtype)

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
train_err_data[user_id == tmp_user_id, .N, by=.(errtype)]



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
##1) 전체 데이터 대상
tmp <- train_err_data[,.N,by=.(year,month,day,hour)][order(year,month,day,hour)]
tmp[,ymd := ymd_h(paste(year,month,day,hour ,sep = '-'))]
g <- ggplot(tmp, aes(x=ymd, y=N)) + geom_line(size=0.3) + 
  scale_x_datetime(breaks = '1 days', date_labels = '%d') +
  theme_bw() + labs(title='날짜 - 시간 별 err 개수')
ggplotly(g)
#11월28일 23시 ~ 11월29일 0시에 급격하게 증가함 
tmp[order(-N)]

## 2) outlier로 보이는 user_id제거한 데이터 대상 
## -> 기존 그래프와 모양이 많이 다름. 데이터 개수가 많은 user_id는 outlier로 보는게 맞을 듯
tmp <- train_err_sub_data[,.N,by=.(year,month,day,hour)][order(year,month,day,hour)]
tmp[,ymd := ymd_h(paste(year,month,day,hour ,sep = '-'))]
g <- ggplot(tmp, aes(x=ymd, y=N)) + geom_line(size=0.3) + 
  scale_x_datetime(breaks = '1 days', date_labels = '%d') +
  theme_bw() + labs(title='날짜 - 시간 별 err 개수')
ggplotly(g)


#3. model_nm : model0~model_8까지 9개 존재
##1) 전체 데이터
tmp1 <- train_err_data[,.N,by=.(model_nm)][order(-N)]
tmp1[,TOTAL := sum(N)]
tmp1[,PER := round(N/TOTAL,2)]
tmp1[,type := 'TOTAL']
ggplot(tmp1, aes(x=model_nm, y=N)) + geom_bar(stat='identity') + theme_bw() + labs(title='model_nm별 데이터 개수')

##2) 일부 데이터
tmp2 <- train_err_sub_data[,.N,by=.(model_nm)][order(-N)]
tmp2[,TOTAL := sum(N)]
tmp2[,PER := round(N/TOTAL,2)]
tmp2[,type := 'SUB']
ggplot(tmp2, aes(x=model_nm, y=N)) + geom_bar(stat='identity') + theme_bw() + labs(title='model_nm별 데이터 개수')

tmp <- rbind(tmp1, tmp2)
ggplot(tmp, aes(x=model_nm, y=PER, fill=type)) + geom_bar(stat='identity', position = 'dodge') +
  theme_bw() + labs(title='Data Type별 model_nm 데이터 비율 ')

#4. fwver : 37종류, 뭔지 모르겠음
length(train_err_data[,unique(fwver)])
train_err_data[,.N,by=.(fwver)][order(-N)]



#5. errtype : 41개의 errtype
##1) 전체 데이터
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

##2) 일부데이터
length(train_err_sub_data[,unique(errtype)])
count_by_errtype <- train_err_sub_data[,.N,by=.(errtype)][order(-N)]
count_by_errtype[,errtype := factor(errtype)]
count_by_errtype[,errtype := fct_reorder(errtype, N)]
ggplot(count_by_errtype, aes(x=errtype, y=N)) + geom_bar(stat='identity') +coord_flip() +theme_bw() +
  labs(title = 'errtype 별 data 개수')

#errttype별 시간별 data 개수 -> 11월29에 errtype 10이 많이 발생 
errtype_time_data_count <- train_err_sub_data[,.N,by=.(errtype, year, month, day, hour)]
errtype_time_data_count[,ymd := ymd_h(paste(year,month,day,hour ,sep = '-'))]
errtype_time_data_count[,errtype := factor(errtype)]
g <- ggplot(errtype_time_data_count, aes(x=ymd, y=N, col=errtype)) + geom_line(size=0.3) + theme_bw() +
  labs(title='errtype별 시간별 데이터 개수')
ggplotly(g)



# 22, 23번에 속한 errcode 종류 및 개수 
# -> 뭔가 connection 관련 에러가 많아보임
train_err_data[errtype %in% c(22,23), .N, by=.(errcode)]
train_err_data[errtype %in% c(23), .N, by=.(errcode)][order(-N)]
train_err_data[errtype %in% c(22), .N, by=.(errcode)][order(-N)]
# 23번이 connection 관련 에러인듯
train_err_data[str_detect(errcode, 'connection'), .N, by=.(errtype)]

# 시간대 별 errtype 23이 발생한 횟수 그래프
ggplot(train_err_data[errtype==23, .N, by=.(ymd_h)], aes(x=ymd_h, y=N)) + geom_line()

#6. errcode : 2803개의 errcode
length(train_err_data[,unique(errcode)])
train_err_data[,.N,by=.(errcode)][order(-N)][1:100]




#################################################################################################################
# train_quality_data EDA : quality data의 수치에 무슨 의미가 있는 듯 
#################################################################################################################
#long format도 추가
train_quality_data_melt <- melt(train_quality_data, id.vars = c('time','user_id','fwver'), variable.name = 'quality')

#10분단위 데이터인듯  
head(train_quality_data)
unique(train_quality_data[,.(time)][order(time)])

#train_err_data의 user_id는 15000개인데 train_quality_data의 user_id는 8281개
length(unique(train_quality_data[,user_id]))
# 전체 problem user_id 5000개 중 3167개가 train_quality_data에 존재
problem_user_id <- unique(train_problem_data[,user_id])
sum(problem_user_id %in% unique(train_quality_data[,user_id]))

 
# user_id 10000번 기준으로 살펴봄
# -> train_err_data는 11월 1일~11월 30일 까지 있는데 train_quality_data는 29,30만 존재
train_quality_data[user_id == 24934]
train_err_data[user_id == 10000]

#quailty별 unique한 값 개수
train_quality_data[,lapply(lapply(.SD,unique),length),.SDcols = c(paste0('quality_',0:12))]

# 시간 기준으로 summary 후 그래프
# quality 11이 뭔가 있어보임
tmp <- train_quality_data[,lapply(.SD, sum, na.rm = T), by=.(time), .SDcols = c(paste0('quality_',0:12))][order(time)]
for(tmp_quality in c(paste0('quality_',1:12))){
  g <- ggplot(tmp, aes_string(x='time', y=tmp_quality)) + geom_line() + labs(title = tmp_quality)
  print(g)
}



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

# 시간 별 불만 접수 개수
# -> 점심 시간대에 가장 접수 개수가 많음
train_problem_data[,.N,by=.(hour)][order(-N)]

#################################################################################################################
# 불만 접수한 유저와 접수하지 않은 유저 비교 
#################################################################################################################
# 전체 problem user_id
problem_user_id <- unique(train_problem_data[,user_id])

# # problem이 1인 user id
# problem_user_id <- train_problem_data[,.N,by=.(user_id)][N==1, user_id]

train_err_data_with_problem <- train_err_sub_data[user_id %in% problem_user_id]
train_err_data_without_problem <- train_err_sub_data[!user_id %in% problem_user_id]

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


#errtype23내의 errcode에서도 차이가 있는지 확인
# 불만 접수한 유저
count_by_errtype1 <- train_err_data_before_problem[errtype == 23,.N,by=.(errcode)]
count_by_errtype1[,TOTAL_COUNT := sum(N)]
count_by_errtype1[,PER := round(N/TOTAL_COUNT, 2)]
count_by_errtype1 <- count_by_errtype1[order(-N)]
count_by_errtype1[,errcode := factor(errcode)]
count_by_errtype1[,errcode := fct_reorder(errcode, N)]
ggplot(count_by_errtype1, aes(x=errcode, y=N)) + geom_bar(stat='identity') +coord_flip() +theme_bw() +
  labs(title = 'errcode 별 data 개수')

count_by_errtype1[,problem := 'Y']

# 불만 접수하지 않은 유저
count_by_errtype2 <- train_err_data_without_problem[errtype == 23,.N,by=.(errcode)]
count_by_errtype2[,TOTAL_COUNT := sum(N)]
count_by_errtype2[,PER := N/TOTAL_COUNT]
count_by_errtype2 <- count_by_errtype2[order(-N)]
count_by_errtype2[,errcode := factor(errcode)]
count_by_errtype2[,errcode := fct_reorder(errcode, N)]
ggplot(count_by_errtype2, aes(x=errcode, y=N)) + geom_bar(stat='identity') +coord_flip() +theme_bw() +
  labs(title = 'errcode 별 data 개수')

count_by_errtype2[,problem := 'N']

# 불만 접수한 유저와 하지 않은 유저 합쳐서 비교
# connection timeout과 connection fail to establish의 비율이 상대적으로 높음
count_by_errtype_total = rbind(count_by_errtype1, count_by_errtype2)
ggplot(count_by_errtype_total, aes(x=errcode, y=PER, fill=problem)) +
  geom_bar(stat = 'identity', position = 'dodge') + coord_flip() +theme_bw() + labs(title = '불만 유무에 따른 errtype 비율')

