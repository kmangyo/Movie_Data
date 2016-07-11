
# 주간 영화 매출 데이터 만들기
daily_df_et$week<-strftime(daily_df_et$day,format="%Y-%W") 
daily_df_et_wk<- daily_df_et %>% group_by(week, value.movieNm, value.movieCd) %>% summarise(sales=sum(value.salesAmt))
daily_df_et_wk_sales<- daily_df_et %>% group_by(week) %>% summarise(sales=sum(value.salesAmt))
daily_df_et_wk<-merge(daily_df_et_wk, daily_df_et_wk_sales, c('week'),all.x=T)
names(daily_df_et_wk)[4:5]<-c('sales','week.sales')

ggplot(daily_df_et_wk_sales, aes(x=as.factor(week), y=sales, group=1)) + geom_line()

# 매출 균등성 (entropy)계산하기
daily_df_et_wk$log<-with(daily_df_et_wk, log(sales/week.sales))
daily_df_et_wk$dv<-with(daily_df_et_wk, sales/week.sales)
daily_df_et_wk$multi<-with(daily_df_et_wk, log*dv)
daily_df_et_wk_sum<-daily_df_et_wk %>% group_by(week) %>% summarise(et=-sum(multi))

ggplot(daily_df_et_wk_sum, aes(x=as.factor(week), y=et, group=1)) + geom_line()

# 주간 매출데이터와 매출 균등성 데이터 merge
daily_df_et_wk_sum<-merge(daily_df_et_wk_sum, daily_df_et_wk_sales, c('week'),all.x=T)
ggplot(daily_df_et_wk_sum, aes(et, sales)) + geom_point()
with(daily_df_et_wk_sum, cor(et,sales))

# 주간 영화 매출 순위 및 비중
daily_df_et_wk$seq<-1
daily_df_et_wk$seq<-with(daily_df_et_wk, ave(seq,week,FUN=cumsum))
daily_df_et_wk$ratio<-with(daily_df_et_wk, sales/week.sales)

ggplot(subset(daily_df_et_wk, seq==10),aes(ratio, week.sales)) + geom_point() + ylab('weekly.sales') + xlab('movie.ratio') + ggtitle('The 10th rank movie sales ratio in a week')
ggplot(subset(daily_df_et_wk, seq==1),aes(ratio, week.sales)) + geom_point() + ylab('weekly.sales') + xlab('movie.ratio') + ggtitle('The 1st rank movie sales ratio in a week')

# 주간 데이터에 월 정보 추가
daily_df_et$mon<-strftime(daily_df_et$day,format="%m") 
daily_mon_week<-daily_df_et[c('week','mon')]
daily_mon_week<-daily_mon_week[!duplicated(daily_mon_week[1]),]
daily_df_et_wk_sum<-merge(daily_df_et_wk_sum, daily_mon_week, c('week'),all.x=T)

ggplot(daily_df_et_wk_sum, aes(factor(mon), sales))+geom_boxplot()
ggplot(daily_df_et_wk_sum, aes(factor(mon), et))+geom_boxplot()

# 계절 더미 변수 봄 기준, 여름, 가을, 겨울을 변수로 사용
daily_df_et_wk_sum$summer<-with(daily_df_et_wk_sum, ifelse(mon=='06'|mon=='07'|mon=='08',1,0))
daily_df_et_wk_sum$autumn<-with(daily_df_et_wk_sum, ifelse(mon=='09'|mon=='10'|mon=='11',1,0))
daily_df_et_wk_sum$winter<-with(daily_df_et_wk_sum, ifelse(mon=='12'|mon=='01'|mon=='02',1,0))

# 베이지안 회귀분석
#http://mcmcpack.berkeley.edu/
library(MCMCpack)
model1 <- MCMCregress(sales ~ et, daily_df_et_wk_sum)
model2 <- MCMCregress(sales ~ summer+autumn+winter, daily_df_et_wk_sum)
model3 <- MCMCregress(sales ~ et+summer+autumn+winter, daily_df_et_wk_sum)
summary(model1);plot(model1)
summary(model2);plot(model2)
summary(model3);plot(model3)

# 모형 비교
#http://bayesfactorpcl.r-forge.r-project.org/
library(BayesFactor)
BF<-regressionBF(sales ~ et+summer+autumn+winter, data = daily_df_et_wk_sum)
head(BF, n=3)
tail(BF, n=3)
BF[14]/BF[1]
BF[15]/BF[14]
