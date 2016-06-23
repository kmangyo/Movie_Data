## 영화진흥위원회 오픈 API를 활용한 일자별 영화 매출 데이터 수집

- 데이터 수집에는 R을 활용하였습니다.
- 영진위 오픈 API에 회원가입을 하면 API key를 제공해 줍니다. 
  - 사용제한은 일 3,000회.
- API 덕분에 쉽게 데이터 수집이 가능하므로 2010-01-01 ~ 2016-05-31 약 6년이 넘는 기간동안의 일자별 데이터를 수집해 보겠습니다. 
  - 2,343건 요청이니 일 사용제한을 넘지는 않습니다.

- 사용한 패키지는 데이터 전처리를 위한 패키지와(plyr, dplyr, reshape2), json 데이터를 처리하기 위한 패키지 rjson을 사용하였습니다. 또한, 시각화를 위한 ggplot2도 사용하였습니다.
```
library(rjson)
library(reshape2)
library(plyr)
library(dplyr)
library(ggplot2)
```
- 필요한 패키지가 준비되었으면, 데이터 수집을 위한 Request URL을 생성합니다.
- Request URL 구조는 기본 URL 사이에 API Key를 넣고 마지막에 수집할 데이터의 일자를 입력합니다. 
  - json으로 데이터를 받아왔지만 XML로 받아 올 수도 있습니다.
  - 예시) 	http://www.kobis.or.kr/kobisopenapi/webservice/rest/boxoffice/searchDailyBoxOfficeList.json?key=430156241533f1d058c603178cc3ca0e&targetDt=20120101
```
api_key<-'영진위에서 발급받은 API 키를 입력합니다.'

dates<-as.numeric(as.Date(c('2010-01-01','2016-05-31')))
dates<-c(dates[1]:dates[2])
dates<-as.Date(dates, origin='1970-01-01')
dates<-as.character(dates)
dates<-gsub("-","",dates)
day<-dates
url<-paste0('http://www.kobis.or.kr/kobisopenapi/webservice/rest/boxoffice/searchDailyBoxOfficeList.json?key=',api_key,'&targetDt=',day)
```
- rjson 패키지를 활용해 데이터를 받아옵니다.
```
url_json<-list()
for(i in 1:length(url)){
url_json[i] <- fromJSON(file=url[i])
  }
```
- 아래와 같은 형식으로 데이터를 받아옵니다. 분석하기에는 여러모로 불편하니 몇 가지 데이터 전처리가 필요합니다.
```
[[1]]
[[1]]$boxofficeType
[1] "일별 박스오피스"

[[1]]$showRange
[1] "20100101~20100101"

[[1]]$dailyBoxOfficeList
[[1]]$dailyBoxOfficeList[[1]]
[[1]]$dailyBoxOfficeList[[1]]$rnum
[1] "1"

[[1]]$dailyBoxOfficeList[[1]]$rank
[1] "1"

[[1]]$dailyBoxOfficeList[[1]]$rankInten
[1] "0"

[[1]]$dailyBoxOfficeList[[1]]$rankOldAndNew
[1] "OLD"

[[1]]$dailyBoxOfficeList[[1]]$movieCd
[1] "20090834"

[[1]]$dailyBoxOfficeList[[1]]$movieNm
[1] "아바타"

[[1]]$dailyBoxOfficeList[[1]]$openDt
[1] "2009-12-17"

[[1]]$dailyBoxOfficeList[[1]]$salesAmt
[1] "4632694000"

[[1]]$dailyBoxOfficeList[[1]]$salesShare
[1] "44.9"

[[1]]$dailyBoxOfficeList[[1]]$salesInten
[1] "1740212000"
...
```
- 필요없는 정보를 제외하고, list형태로 데이터를 한번 정제해 주도록 하겠습니다.
```
daily_df<-list()
for(i in 1:length(url_json)){
  daily_df[[i]]<-melt(url_json[i])
  daily_df[[i]]<-daily_df[[i]][c(-4,-5)]
  daily_df[[i]]<-reshape(daily_df[[i]], idvar="L3", timevar="L4", direction="wide")
  daily_df[[i]]<-daily_df[[i]][complete.cases(daily_df[[i]][,3]),]
}
```
- 아래와 같이 보다 깔끔하게 데이터를 확인할 수 있게 되었습니다.
```
[[1]]
    L3 value.NA value.rnum value.rank value.rankInten value.rankOldAndNew value.movieCd                              value.movieNm value.openDt value.salesAmt value.salesShare value.salesInten value.salesChange
3    1     <NA>          1          1               0                 OLD      20090834                                     아바타   2009-12-17     4632694000             44.9       1740212000                60
21   2     <NA>          2          2               0                 OLD      20090077                                     전우치   2009-12-23     2713793500             26.3       1118418500                70
39   3     <NA>          3          3               0                 OLD      20090856                                  셜록 홈즈   2009-12-23     1245758500             12.1        495410000                70
57   4     <NA>          4          4               0                 OLD      20090891                                       나인   2009-12-31      746695000              7.2        212079500                40
75   5     <NA>          5          5               0                 OLD      20090861                           앨빈과 슈퍼밴드2   2009-12-30      603097000              5.8        257932000                70
93   6     <NA>          6          6               0                 OLD      20090860 포켓 몬스터 DP: 아르세우스 초극의 시공으로   2009-12-24      144107000              1.4         61325500                70
111  7     <NA>          7          7               0                 OLD      20090089                 파르나서스 박사의 상상극장   2009-12-23      132574000              1.3         48084000                60
129  8     <NA>          8          8               1                 OLD      20090859                            러브 매니지먼트   2009-12-31       17261000              0.2          6099500                50
147  9     <NA>          9          9               2                 OLD      20090685                                       2012   2009-11-12       14222000              0.1          9924500               230
165 10     <NA>         10         10              -2                 OLD      20090840                                위대한 침묵   2009-12-03       13771000              0.1          2511500                20
    value.salesAcc value.audiCnt value.audiInten value.audiChange value.audiAcc value.scrnCnt value.showCnt
3      48073858500        542043          216989               70       5692710           707          3424
21     20461395500        364880          141516               60       2818763           549          2956
39      9488220000        165556           60924               60       1296997           356          1807
57      1429331000         95219           21872               30        188433           331          1870
75      1304775000         86555           33188               60        192318           243          1083
93      1424646000         20772            7698               60        214790            61           253
111     2987361000         17547            5641               50        411621           245           632
129       29910500          2166             623               40          3895            41           194
147    38901105500          1983            1316              200       5392184            18            39
165      183151000          1726             139               10         25982             6            22
```
- 하지만 여전히 list 형태이므로, 분석하기 편하게 data.frame으로 변환하는 작업을 진행하도록 하겠습니다.
- plyr의 rbind.fill 함수를 사용하면, 리스트 형태를 하나의 data.frame으로 합칠 수 있습니다.
```
df <- rbind.fill(daily_df)
daily_df<-df
```
- 각각 개별 변수에 대한 설명은 [영진위 오픈API 사이트] (http://www.kobis.or.kr/kobisopenapi/homepg/apiservice/searchServiceInfo.do)에서 확인할 수 있습니다.
- data.frame으로 합쳐진 상태에서는 날짜정보가 포함되어 있지 않으니, 날짜정보를 생성하여 넣어주도록 합니다.
```
df_day<-as.Date(c(14610:16952), origin='1970-01-01')
df_day<-data.frame(day=df_day,L1=c(1:2343))
daily_df$L1<-rep(1:2343,each=10)
daily_df<-merge(daily_df, df_day, c('L1'),all.x=T)
```
- 이제 일자마다 상위 10개 영화의 매출액과 매출 비중 데이터를 합해서 일자별 데이터를 만들 수 있습니다.
- 그리고 상위 10개의 해당 일자 매출 비중을 알 수 있으니, 당연히 상위 10개에 포함되지 않는 매출액도 계산할 수 있습니다.
```
daily_sales<- daily_df %>% group_by(day) %>% summarise(sum.sales=sum(value.salesAmt),sum.share=sum(value.salesShare)
daily_sales$total.sales<-with(daily_sales, sum.sales/sum.share*100)
```
- 데이터 정제가 완료되었습니다. 아래처럼 일자별 매출을 확인할 수 있습니다.
```
        day   sum.sales sum.share total.sales
1 2010-01-01 10263973000      99.4 10325928571
2 2010-01-02  9974429000      99.4 10034636821
3 2010-01-03  7915739000      99.3  7971539778
4 2010-01-04  2789491000      99.4  2806328974
5 2010-01-05  2689125000      98.9  2719034378
6 2010-01-06  2478304000      98.6  2513492901
```
- 마지막으로 일자별 매출 추이를 그려보도록 하겠습니다.
```
daily_sales<-data.frame(daily_sales)
ggplot(daily_sales, aes(x=day, y=total.sales)) + geom_line() + xlab(c('날짜')) +ylab(c('매출'))
```
![사용자 입력](https://dl.dropboxusercontent.com/u/1049842/%EB%B8%94%EB%A1%9C%EA%B7%B8/%EC%98%81%ED%99%94_%EB%8B%A4%EC%9D%8C/movie_sales_data.png)

- 시즈널(주간, 월간 등)한 특성을 가진 완만한 트랜드의 매출추이를 확인할 수 있습니다.
- 데이터 수집은 여기까지 입니다. 간단한 분석은 [저의 개인 블로그] (http://khg423.dothome.co.kr/index.php/2016/06/21/%EC%98%81%ED%99%94-%EB%A7%A4%EC%B6%9C-%EB%8D%B0%EC%9D%B4%ED%84%B0-%EC%82%B4%ED%8E%B4%EB%B3%B4%EA%B8%B0-%EC%98%81%ED%99%94-%EB%8D%B0%EC%9D%B4%ED%84%B0-%EB%B6%84%EC%84%9D-part-2/)에 올려놓았으니 관심 있으신 분은 방문해 주세요.
