library(rjson)
library(reshape2)
library(ggplot2)
library(dplyr)
library(tidyr)

api_key<-'영진위에서 발급받은 API 키를 입력합니다.'

dates<-as.numeric(as.Date(c('2010-01-01','2016-05-31')))
dates<-c(dates[1]:dates[2])
dates<-as.Date(dates, origin='1970-01-01')
dates<-as.character(dates)
dates<-gsub("-","",dates)
day<-dates
url<-paste0('http://www.kobis.or.kr/kobisopenapi/webservice/rest/boxoffice/searchDailyBoxOfficeList.json?key=',api_key,'&targetDt=',day)

url_json<-list()
for(i in 1:length(url)){
url_json[i] <- fromJSON(file=url[i])
  }

daily_df<-list()
for(i in 1:length(url_json)){
  daily_df[[i]]<-melt(url_json[i])
  daily_df[[i]]<-daily_df[[i]][c(-4,-5)]
  daily_df[[i]]<-reshape(daily_df[[i]], idvar="L3", timevar="L4", direction="wide")
  daily_df[[i]]<-daily_df[[i]][complete.cases(daily_df[[i]][,3]),]
}

# Data.frame으로 변환하는 작업, 시간이 다소 걸립니다.
df <- rbind.fill(daily_df)
daily_df<-df

daily_df$value.salesShare<-as.numeric(as.character(daily_df$value.salesShare))
daily_df$value.audiCnt<-as.numeric(as.character(daily_df$value.audiCnt))
daily_df$value.salesAmt<-as.numeric(as.character(daily_df$value.salesAmt))

df_day<-as.Date(c(14610:16952), origin='1970-01-01')
df_day<-data.frame(day=df_day,L1=c(1:2343))
daily_df$L1<-rep(1:2343,each=10)
daily_df<-merge(daily_df, df_day, c('L1'),all.x=T)

# 요일별 개별 영화의 매출 분포 box-plot.
daily_df$weekdays<-weekdays(daily_df$day)
daily_df$weekdays <- factor(daily_df$weekdays, levels = c("Monday", "Tuesday", "Wednesday", "Thursday","Friday","Saturday","Sunday"))
ggplot(daily_df, aes(factor(weekdays), value.salesAmt))+geom_boxplot()

# 일자별 매출 데이터.
daily_sales<- daily_df %>% group_by(day) %>% summarise(sum.sales=sum(value.salesAmt),sum.share=sum(value.salesShare))
ggplot(daily_sales, aes(x=day, y=sum.sales)) + geom_line()
ggplot(daily_sales, aes(x=day, y=sum.share)) + geom_line()
quantile(daily_sales$sum.share, c(1:10)*.1)

# 일자별 매출 데이터 상위 10위 제외한 영화 매출 합 구하기. 
daily_sales$total.sales<-with(daily_sales, sum.sales/sum.share*100)
daily_sales$rest<-with(daily_sales, total.sales-sum.sales)

# 수집 데이터 기간 동안 일자별 전체 영화 매출 추이 살펴보기.
daily_sales<-data.frame(daily_sales)
ggplot(daily_sales, aes(x=day, y=total.sales)) + geom_line()

# 월별 매출 분포 box-plot. 
daily_sales$date<-as.POSIXlt(as.character(daily_sales$day))
daily_sales$month<-daily_sales$date$mon
ggplot(daily_sales, aes(factor(month), total.sales))+geom_boxplot()

# 년도별 매출 분포 box-plot. 
daily_sales$year<-daily_sales$date$year
ggplot(daily_sales, aes(factor(year), total.sales))+geom_boxplot()
daily_sales$date<- as.character(daily_sales$date)

# 분해법을 사용하여 시계열 데이터의 시즈널과 트랜드 나머지를 분리
# 참고) http://www.r-bloggers.com/seasonal-trend-decomposition-in-r/

daily_sales_ts <- ts(daily_sales$total.sales, start=c(2010,1), frequency =365)
daily_sales_ts.stl <- stl(daily_sales_ts, s.window="periodic")
plot(daily_sales_ts.stl)

daily_sales_ts.decomp<-daily_sales_ts.stl$time.series
daily_sales_ts.decomp<-data.frame(daily_sales_ts.decomp)

daily_sales_ts.decomp$time<-c(1:nrow(daily_sales_ts.decomp))
daily_sales_ts.decomp<-melt(daily_sales_ts.decomp, id=c('time'))
ggplot(data=daily_sales_ts.decomp, aes(x=time, y=value, colour=variable)) + geom_line()
ggplot(data=subset(daily_sales_ts.decomp,time>=2000), aes(x=time, y=value, colour=variable)) + geom_line()

# 개별 영화의 개봉일 이후 지난일자를 계산
daily_df$value.openDt<-as.Date(as.character(daily_df$value.openDt))
daily_df$day_dif<-with(daily_df, day-value.openDt)

# 개봉일 이후 60일 이내의 데이터 매출 분포 box-plot.
ggplot(subset(daily_df,day_dif>=0&day_dif<=60), aes(factor(day_dif), value.salesAmt))+geom_boxplot()
