library(rvest)
library(stringi)
library(reshape2)
library(dplyr)
library(ggplot2)

#영진위의 매출 데이터를 기준으로 리뷰 데이터를 수집
#인코딩 이슈가 있는 경우 아래 함수를 사용
#iconv(names(movie.sales_open), "CP949", "UTF-8")

#리뷰검색을 위한 영화명 정리
movie.name<-movie.sales_open$영화명
movie.name<-toupper(movie.name)
movie.name<-gsub("\\s", "", movie.name)

movie.name.number<-data.frame(name=movie.name, num=c(1:length(movie.name)))

#다음 영화 검색에 해당 영화명을 쿼리로 사용하여 2015년 개봉 영화 찾기
url<-paste0('http://movie.daum.net/search.do?type=movie&q=',movie.name)

#쿼리에 대응하는 검색결과
MovieTitles<-NA
for(i in 1:length(url)){
  MovieTitles[i]<-list(url[i] %>% read_html(url[i]) %>% html_nodes(".fs13") %>% html_text())
}

MovieTitles.melt<-melt(MovieTitles)

#쿼리에 대응하는 검색결과로 나오는 개별 영화의 URL
MovieURL<-NA
for(i in 1:length(url)) {
  MovieURL[i]<-list(url[i] %>% read_html(url[i]) %>% html_nodes(".fs13") %>% html_attr("href"))
}

MovieURL.melt<-melt(MovieURL)

MovieURL.melt<-merge(MovieURL.melt, movie.name.number, c('num'),all.x=T)
MovieTitles.melt<-merge(MovieTitles.melt, movie.name.number, c('num'),all.x=T)

#개별 영화 URL에서 다음내에서의 해당 영화 Id 찾기
MovieURL.melt$movieId<-gsub("[[:print:]]+(movieId=)", "", MovieURL.melt$value)
MovieURL.melt.id<-MovieURL.melt[complete.cases(MovieURL.melt[,4]),]

#영화 Id를 사용하여 해당 영화의 개봉일자, 평점, 평점을 준 사람 수를 분류
#비효율적인 방법이지만, 좋은 아이디어가 떠오르지는 복잡하게 작업을 진행함

movieID.URL<-paste0("http://movie.daum.net/moviedetail/moviedetailNetizenPoint.do?movieId=",MovieURL.melt.id$movieId,"&t__nil_main_NetizenPoint=more")

movie.Info<-NA
for(i in 1:length(movieID.URL)) {
  movie.Info[i]<-list(movieID.URL[i] %>% read_html(movieID.URL[i]) %>% html_nodes("#movieinfoDetail") %>% html_text())
}
movie.Info.melt<-melt(movie.Info)
movie.head.info<-cbind(MovieURL.melt.id,movie.Info.melt)
names(movie.head.info)[5]<-'info'
movie.head.info$info<-gsub("\t|\r", "", movie.head.info$info)
movie.head.info$info<-gsub("\n\n", "", movie.head.info$info)
movie.head.info$info<-gsub("\\|", "", movie.head.info$info)

movie.head.info.split<-list(strsplit(as.character(movie.head.info$info),'\n',fixed=T))
movie.head.info.split<-melt(movie.head.info.split)

movie.head.info.split$value<-as.character(movie.head.info.split$value)
movie.head.info.split$value<-gsub("\\s", "", movie.head.info.split$value)
movie.head.info.split$nchar<-nchar(movie.head.info.split$value)
movie.head.info.split<-subset(movie.head.info.split, nchar>0)
movie.head.info.split<-movie.head.info.split[c(-4)]

movie.head.info.split$day<-stri_count_regex(movie.head.info.split$value, "2015-")
movie.head.info.split.date<-movie.head.info.split %>% group_by(L1, L2) %>% summarise(open.day=sum(day))
movie.head.info.split.date<-subset(movie.head.info.split.date, open.day>0)

movie.head.info.split.date<-merge(movie.head.info.split.date, movie.head.info.split, c('L1','L2'), all.x=T)
movie.head.info.split.date$nostar<-stri_count_regex(movie.head.info.split.date$value, "평점주기")
movie.head.info.split.date$star<-stri_count_regex(movie.head.info.split.date$value, "네티즌별점")

movie.head.info.split.date$sum<-with(movie.head.info.split.date, day+nostar+star)
movie.head.info.split.date.sum<-subset(movie.head.info.split.date, sum>0)
movie.head.info.split.date.sum<-movie.head.info.split.date.sum %>% arrange(L2, day,nostar,star) 

movie.head.info.split.date.sum$col<-with(movie.head.info.split.date.sum, ifelse(day==1,c('open'),ifelse(nostar==1, c('numberfostar'),c('star'))))
movie.head.info.split.date.sum.reshape<-reshape(movie.head.info.split.date.sum, idvar="L2", timevar="col", direction="wide")
movie.head.info.split.date.sum.reshape<-movie.head.info.split.date.sum.reshape[c(1,4,11,18)]
names(movie.head.info.split.date.sum.reshape)[1]<-'L1'

movie.head.info.reshape<-merge(movie.head.info, movie.head.info.split.date.sum.reshape, c('L1'),all.x=T)
movie.head.info.reshape.com<-movie.head.info.reshape[complete.cases(movie.head.info.reshape[,9]),]
movie.head.info.reshape.com$value.open<-as.character(movie.head.info.reshape.com$value.open)
movie.head.info.reshape.com$value.open<-as.Date(movie.head.info.reshape.com$value.open)

#영화명과 영화 개봉일자가 일치하는 데이터를 합침
movie.sales_open$name<-gsub("\\s", "", movie.sales_open$영화명)
movie.sales_open.review<-merge(movie.sales_open, movie.head.info.reshape.com, c('name','date'),all=F)

movie.sales_open.review$sales<-as.character(movie.sales_open.review$매출액)
movie.sales_open.review$sales<-gsub(",", "", movie.sales_open.review$sales)
movie.sales_open.review$sales<-as.numeric(movie.sales_open.review$sales)

movie.sales_open.review$star<-stri_sub(movie.sales_open.review$value.star,-(nchar(movie.sales_open.review$value.star)-5),-1)
movie.sales_open.review$numberfostar<-gsub("명참여)평점주기", "", movie.sales_open.review$value.numberfostar)
movie.sales_open.review$numberfostar<-gsub("\\(", "", movie.sales_open.review$numberfostar)

movie.sales_open.review$star<-as.numeric(as.character(movie.sales_open.review$star))
movie.sales_open.review$numberfostar<-as.numeric(as.character(movie.sales_open.review$numberfostar))

movie.sales_open.review$ID<-1
movie.sales_open.review$ID<-cumsum(movie.sales_open.review$ID)

# 개별 영화의 140자 리뷰 데이터를 수집. 리뷰 텍스트와 평점, 리뷰일자 데이터
movie.sales_open.review$pages<-with(movie.sales_open.review, ceiling(numberofstar/15))
Movie.urls<-paste0("http://movie.daum.net/moviedetail/moviedetailNetizenPoint.do?movieId=",movie.sales_open.review$movieId,"&searchType=all&type=after&page=")

pages<-movie.sales_open.review$pages
Movie.urls.pages<-list()

for( i in 1:length(Movie.urls)){
Movie.urls.pages[[i]]<-paste0(Movie.urls[i],c(1:pages[i]))
}

Movie.urls.pages<-melt(Movie.urls.pages)

Movie.urls.pages$value<-as.character(Movie.urls.pages$value)
Movie.urls.pages.urls <- lapply(Movie.urls.pages$value,read_html)

Movie.urls.review<-NA
Movie.urls.star<-NA
Movie.urls.date<-NA

for (i in 1:length(Movie.urls.pages.urls) ) {
  Movie.urls.review[i]<-melt(Movie.urls.pages.urls[[i]] %>% html_nodes(".article") %>% html_text())
  Movie.urls.star[i]<-melt(Movie.urls.pages.urls[[i]] %>% html_nodes("#movieNetizenPointList em") %>% html_text())
  Movie.urls.date[i]<-melt(Movie.urls.pages.urls[[i]] %>% html_nodes(".datetxt") %>% html_text())
}

Movie.urls.review<-melt(Movie.urls.review)
Movie.urls.star<-melt(Movie.urls.star)
Movie.urls.date<-melt(Movie.urls.date)

Movie.urls.df<-cbind(Movie.urls.review, Movie.urls.star, Movie.urls.date)
Movie.urls.df<-Movie.urls.df[c(-2,-4)]
names(Movie.urls.df)<-c('text','star','date','url.no')

Movie.urls.df$star<-as.numeric(as.character(Movie.urls.df$star))
Movie.urls.df$date<-as.character(Movie.urls.df$date)
Movie.urls.df$date<-gsub("\\.", "-", Movie.urls.df$date)
Movie.urls.df$date<-as.POSIXct(Movie.urls.df$date)

Movie.urls.pages$url.no<-1
Movie.urls.pages$url.no<-cumsum(Movie.urls.pages$url.no)

# 기존에 합친 영화 매출, 영화 전체 리뷰데이터에 개별 영화의 리뷰데이터를 합침
Movie.urls.df<-merge(Movie.urls.df, Movie.urls.pages, c('url.no'),all.x=T)

Movie.urls.df$movieId<-gsub("[[:print:]]+(movieId=)", "", Movie.urls.df$value)
Movie.urls.df$movieId<-gsub("(&searchType)+[[:print:]]+$", "", Movie.urls.df$movieId)

movie.sales_open.review.all<-merge(movie.sales_open.review, Movie.urls.df, c('movieId'),all.x=T)
movie.sales_open.review.all<-subset(movie.sales_open.review.all, numberofstar>0)

movie.sales_open.review.all$text.clean<- gsub("\t", " ", movie.sales_open.review.all$text)
movie.sales_open.review.all$text.clean<- gsub("\n", " ", movie.sales_open.review.all$text.clean)
movie.sales_open.review.all$text.clean<- gsub("\r", " ", movie.sales_open.review.all$text.clean)
movie.sales_open.review.all$text.clean<- gsub("\\s+", " ", movie.sales_open.review.all$text.clean)
